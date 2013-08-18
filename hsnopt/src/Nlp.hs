{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}
{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Nlp ( Nlp
           , (===)
           , (<==)
           , minimize
           , designVar
           , solveNlp
           ) where

import Control.Lens ( (^.), makeLenses, over, set )
import Control.Monad ( unless, when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState, get, put )
import Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HM
import qualified Data.Sequence as S
import Data.Sequence ( (|>) )
import Foreign.Ptr
import Foreign.Storable

import LlvmAd
import Snopt

import qualified Dvda
import Dvda ( Expr )
import Dvda.Llvm

import LogsAndErrors

--withEllipse :: Int -> String -> String
--withEllipse n blah
--  | length blah <= n = blah
--  | otherwise = take n blah ++ "..."

data Objective a = ObjectiveUnset | Objective a

data Constraint a = Eq2 a a
                  | Ineq2 a a
                  | Ineq3 a a a

data NlpState a = NlpState { _nlpX :: HM.HashMap String a
                           , _nlpXNames :: S.Seq String
                           , _nlpConstraints :: S.Seq (Constraint a)
                           , _nlpObj :: Objective a
                           , _nlpVarPool :: [a]
                           , _nlpNumVars :: Int
                           }
makeLenses ''NlpState

newtype Nlp s a =
  Nlp
  { runNlp :: ErrorT ErrorMessage (WriterT [LogMessage] (State (NlpState s))) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState (NlpState s)
             , MonadWriter [LogMessage]
             )

build :: NlpState s -> Nlp s a -> (Either ErrorMessage a, [LogMessage], NlpState s)
build nlp0 builder = (result, logs, state)
  where
    ((result,logs),state) =
      flip runState nlp0 . runWriterT . runErrorT . runNlp $ builder

designVar :: String -> Nlp a a
designVar name = do
  debug $ "adding design variable \""++name++"\""
  state0 <- get
  let n = state0 ^. nlpNumVars
      sym = (state0 ^. nlpVarPool) !! n
      map0 = state0 ^. nlpX
      state1 = over nlpXNames (|> name) (set nlpNumVars (n+1) state0)
  when (HM.member name map0) $ err $ name ++ " already in symbol map"
  put $ set nlpX (HM.insert name sym map0) state1
  return sym

infix 4 ===
(===) :: a -> a -> Nlp a ()
(===) lhs rhs = do
  debug $ "adding equality constraint: " -- ++
    --withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ over nlpConstraints (|> Eq2 lhs rhs) state0

infix 4 <==
(<==) :: a -> a -> Nlp a ()
(<==) lhs rhs = do
  debug $ "adding inequality constraint: " -- ++
    -- withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ over nlpConstraints (|> (Ineq2 lhs rhs)) state0

leq3 :: a -> a -> a -> Nlp a ()
leq3 lhs mid rhs = do
  debug $ "adding inequality constraint bounds: " -- ++
--    withEllipse 30 (show lhs) ++ " <= " ++
--    withEllipse 30 (show mid) ++ " <= " ++
--    withEllipse 30 (show rhs)
  state0 <- get
  put $ over nlpConstraints (|> (Ineq3 lhs mid rhs)) state0

minimize :: a -> Nlp a ()
minimize obj = do
  debug $ "setting objective function: " -- ++ withEllipse 30 (show obj)
  state0 <- get
  case state0 ^. nlpObj of
    Objective _ -> err $ init $ unlines $
                   [ "you set the objective function twice"
--                   , "    old val: " ++ show x
--                   , "    new val: " ++ show obj
                   ]
    ObjectiveUnset -> put $ set nlpObj (Objective obj) state0
  

toFun :: Num a => Nlp a () -> [a] -> [(Double,a,Double)]
toFun nlp = f
  where
    f x = case build (NlpState HM.empty S.empty S.empty ObjectiveUnset x 0) nlp of
      (Left er, logs, _) -> error (unlines (map show logs ++ ["NLP error: " ++ show er]))
      (_, _, result) -> (0, obj, 0) : (map constr (F.toList (result ^. nlpConstraints)))
        where
          obj = case result ^. nlpObj of
            (Objective obj') -> obj'
            _ -> error "can't call toFun without setting objective"
          inf = 1e40
          constr (Eq2 lhs rhs) = (0, lhs - rhs, 0)
          constr (Ineq2 lhs rhs) = (-inf, lhs - rhs, 0)
          constr _ = error "don't be greedy now"
        
emptySymbolicNlp :: NlpState (Expr Double)
emptySymbolicNlp = NlpState HM.empty S.empty S.empty ObjectiveUnset
                   (map (\k -> Dvda.sym ('x':show k)) [(0::Int)..] :: [Expr Double]) 0


solveNlp :: (forall a. Floating a => Nlp a ()) -> IO ()
solveNlp nlp = do
  let (_,_,state) = build emptySymbolicNlp nlp
      middle xs = (\(_,x,_) -> x) $ unzip3 xs
      inputs = map Dvda.sym $ F.toList (state ^. nlpXNames) :: [Expr Double]
      objRow = 1
      objAdd = 0
      (flow,_,fupp) = unzip3 $ toFun nlp inputs
      fbnds = zip flow fupp
  (fg, ijxA, ijG) <- doMagic (middle . toFun nlp) inputs
  
  workspace <- mallocWorkspace 500 10000 20000
  snInit workspace
  snX <- makeX (map (\_ -> (0,(-1e30,1e30))) inputs)
  snF <- makeF objRow objAdd fbnds
  (snA,snG) <- makeAG ijxA ijG

  let runSnopt :: FunPtr () -> IO SnInteger
      runSnopt fp0 = do
        let fp = mkIOStub3 (castFunPtr fp0)
            userfg _ _ x' needF _ f' needG _ g' _ _ _ _ _ _ = do
              needF' <- peek needF
              needG' <- peek needG
              unless (needF' `elem` [0,1]) $ error "needF isn't 1 or 0"
              unless (needG' `elem` [0,1]) $ error "needG isn't 1 or 0"
              when (needF' == 1 || needG' == 1) $ do
              _ <- fp (castPtr x') (castPtr f') (castPtr g')
              --_ <- fp (castPtr x') (castPtr f')
              return ()
        userfg' <- wrap userfg
        snopta workspace snX snF snA snG userfg'

  ret <- withLlvmJit fg runSnopt
  putStrLn $ "final ret: " ++ show ret
