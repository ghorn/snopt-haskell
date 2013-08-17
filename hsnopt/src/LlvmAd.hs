{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}


import Control.Monad ( unless, when )
import Data.Either ( partitionEithers )
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector as V
import GHC.Word ( Word32 )
import Numeric.AD
import Numeric.AD.Types

import Snopt

import qualified Dvda
import Dvda.Expr
import Dvda.FunGraph ( FunGraph)
import Dvda.Llvm

foreign import ccall "dynamic" mkIOStub3 ::
  FunPtr (Ptr Double -> Ptr Double -> Ptr Double -> IO Word32)
  -> Ptr Double -> Ptr Double -> Ptr Double -> IO Word32

data WhoAmI a = Nonlinear (Expr a)
              | Linear a
              | Zero
whoAmI :: (Eq a, Num a) => Expr a -> WhoAmI a
whoAmI (EConst 0) = Zero
whoAmI (EConst c) = Linear c
whoAmI (ENum (FromInteger 0)) = Zero
whoAmI (ENum (FromInteger c)) = Linear (fromInteger c)
whoAmI (EFractional (FromRational 0)) = Zero
whoAmI (EFractional (FromRational c)) = Linear (fromRational c)
whoAmI x = Nonlinear x

doMagic ::
  (forall s. Mode s => [AD s (Expr Double)] -> [AD s (Expr Double)])
  -> [Expr Double]
  -> IO ( FunGraph VList VList Double
        , [((SnInteger,SnInteger), SnDoubleReal)]
        , [(SnInteger,SnInteger)]
        )
doMagic f x = do
  let (f0,jac) = unzip $ jacobian' f x
      myjac :: [[Expr Double]]
      myjac = jac

      -- snopt is 1-indexed
      indices = [[(i,j) | j <- take (length x) [1..]] | i <- take (length f0) [1..]]

      splitLinear (x':xs) (ij:ijs) = case whoAmI x' of
        Zero        -> splitLinear xs ijs
        Nonlinear e -> Left  (ij,e):splitLinear xs ijs
        Linear c    -> Right (ij,realToFrac c):splitLinear xs ijs
      splitLinear [] [] = []
      splitLinear [] (_:_) = error "splitLinear: list mismatch"
      splitLinear (_:_) [] = error "splitLinear: list mismatch"
  
      (nljac, linjac) = partitionEithers $ splitLinear (concat myjac) (concat indices)
      (ijG,g) = unzip nljac
      ijxA = linjac

  putStrLn "--------- f ---------"
  print f0
  putStrLn "--------- jacobian f ---------"
  mapM_ print jac
  putStrLn "--------- nonlinaer jacobian f ---------"
  mapM_ print nljac
  putStrLn "--------- linaer jacobian f ---------"
  mapM_ print linjac
  fg <- Dvda.toFunGraph
        (VList (V.singleton (V.fromList x)))
        (VList (V.fromList [V.fromList f0, V.fromList g]))

  return (fg, ijxA, ijG)

main :: IO ()
main = do
  let f [x1,x2] = [                 x2   
                  ,  x1**2      + 4*x2**2
                  , (x1 - 2)**2 +   x2**2
                  ]
      f _ = error "oh come on"
      xsym = map Dvda.sym ["x1","x2"] :: [Expr Double]

      objRow = 1
      objAdd = 0
      
      inf = 1e20
      x = [ (1, (    0, inf))
          , (1, (-inf, inf))
          ]
      fbnds = [ (-inf, inf)
              , (-inf, 4)
              , (-inf, 5)
              ]
      
  (fg, ijxA, ijG) <- doMagic f xsym
  
  workspace <- mallocWorkspace 500 10000 20000
  snInit workspace
  snX <- makeX x
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
