{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Snopt.SnoptA ( SnoptA
                    , SnDoubleReal
                    , SnInteger
                    , runSnoptA
                    , sninit
                    , snseti
                    , snjac
                    , snopta
                      -- * getters/setters
                    , getXlow
                    , getXupp
                    , getX
                    , getXstate
                    , getXmul

                    , setXlow
                    , setXupp
                    , setX
                    , setXstate
                    , setXmul

                    , getFlow
                    , getFupp
                    , getFstate
                    , getFmul
                    , getF
                    , getObjRow
                    , getObjAdd

                    , setFlow
                    , setFupp
                    , setFstate
                    , setFmul
                    , setF
                    , setObjRow
                    , setObjAdd

                    , getIAfun
                    , getJAvar
                    , getA

                    , setIAfun
                    , setJAvar
                    , setA

                    , getIGfun
                    , getJGvar

                    , setIGfun
                    , setJGvar

                    , getIprint
                    , setIprint

                    , getIsummary
                    , setIsummary

                      -- * example
                    , toy1
                    ) where

import Control.Lens ( Getting, Accessor, (^.) )
import Control.Monad ( unless, when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.Reader ( ReaderT, MonadReader, runReaderT, ask )
import Control.Monad.IO.Class
import Foreign.Ptr ( Ptr )
import Foreign.Marshal
import Foreign.Storable

import Snopt.Bindings
import qualified Snopt.Internal as Internal
import Snopt.Internal ( SnoptA', mallocSnoptA, freeSnoptA )

newtype SnoptA a = SnoptA { toSnoptA :: ErrorT String (ReaderT SnoptA' IO) a }
                 deriving ( Monad
                          , MonadIO
                          , MonadError String
                          , MonadReader SnoptA'
                          )
runSnoptA :: Int -> Int -> Int -> Int -> Int -> Int -> Int
          -> U_fp -> SnoptA a -> IO (Either String a)
runSnoptA nc ni nr nx nf na ng ufp userSnoptA = do
  ufp' <- wrap ufp
  sna' <- mallocSnoptA nc ni nr nx nf na ng ufp'
  case sna' of
    Left e -> return (Left e)
    Right sna -> do
      ret <- flip runReaderT sna . runErrorT $ toSnoptA userSnoptA
      freeSnoptA sna
      return ret

--------------------------- setters/getters ----------------------------
getArray :: Storable a =>
            Getting (Ptr SnInteger) xfag (Ptr SnInteger)
         -> ((xfag -> Accessor xfag xfag) -> SnoptA' -> Accessor xfag SnoptA')
         -> Getting (Ptr a) xfag (Ptr a)
         -> SnoptA [a]
getArray getNum getXFAG getField = do
  snoptA <- ask
  let snx  = snoptA ^. getXFAG
  n <- liftIO $ peek (snx ^. getNum)
  liftIO $ peekArray (fromIntegral n) (snx ^. getField)

setArray :: Storable a =>
            String
         -> Getting (Ptr SnInteger) xfag (Ptr SnInteger)
         -> ((xfag -> Accessor xfag xfag) -> SnoptA' -> Accessor xfag SnoptA')
         -> Getting (Ptr a) xfag (Ptr a)
         -> [a]
         -> SnoptA ()
setArray name getNum getXFAG getField val = do
  snoptA <- ask
  let snx  = snoptA ^. getXFAG
  n <- liftIO $ peek (snx ^. getNum)
  let lval = length val
      trueval = fromIntegral n
  when (lval /= trueval) $
    error $ "setArray dimension mismatch: " ++ name ++ ": " ++ show lval ++ " /= " ++ show trueval
  liftIO $ pokeArray (snx ^. getField) val

getScalar :: Storable a =>
             ((xfag -> Accessor xfag xfag) -> SnoptA' -> Accessor xfag SnoptA')
          -> Getting (Ptr a) xfag (Ptr a)
          -> SnoptA a
getScalar getXFAG getField = do
  snoptA <- ask
  let snx  = snoptA ^. getXFAG
  liftIO $ peek (snx ^. getField)

setScalar :: Storable a =>
             ((xfag -> Accessor xfag xfag) -> SnoptA' -> Accessor xfag SnoptA')
          -> Getting (Ptr a) xfag (Ptr a)
          -> a
          -> SnoptA ()
setScalar getXFAG getField val = do
  snoptA <- ask
  let snx  = snoptA ^. getXFAG
  liftIO $ poke (snx ^. getField) val

getXlow :: SnoptA [SnDoubleReal]
getXlow   = getArray Internal.sx_nx Internal.sna_x Internal.sx_xlow
getXupp :: SnoptA [SnDoubleReal]
getXupp   = getArray Internal.sx_nx Internal.sna_x Internal.sx_xupp
getX :: SnoptA [SnDoubleReal]
getX      = getArray Internal.sx_nx Internal.sna_x Internal.sx_x
getXstate :: SnoptA [SnInteger]
getXstate = getArray Internal.sx_nx Internal.sna_x Internal.sx_xstate
getXmul :: SnoptA [SnDoubleReal]
getXmul   = getArray Internal.sx_nx Internal.sna_x Internal.sx_xmul

setXlow :: [SnDoubleReal] -> SnoptA ()
setXlow   = setArray "xlow"   Internal.sx_nx Internal.sna_x Internal.sx_xlow
setXupp :: [SnDoubleReal] -> SnoptA ()
setXupp   = setArray "xupp"   Internal.sx_nx Internal.sna_x Internal.sx_xupp
setX :: [SnDoubleReal] -> SnoptA ()
setX      = setArray "x"      Internal.sx_nx Internal.sna_x Internal.sx_x
setXstate :: [SnInteger] -> SnoptA ()
setXstate = setArray "xstate" Internal.sx_nx Internal.sna_x Internal.sx_xstate
setXmul :: [SnDoubleReal] -> SnoptA ()
setXmul   = setArray "xmul  " Internal.sx_nx Internal.sna_x Internal.sx_xmul

getFlow :: SnoptA [SnDoubleReal]
getFlow   = getArray Internal.sf_nF Internal.sna_f Internal.sf_flow
getFupp :: SnoptA [SnDoubleReal]
getFupp   = getArray Internal.sf_nF Internal.sna_f Internal.sf_fupp
getFstate :: SnoptA [SnInteger]
getFstate = getArray Internal.sf_nF Internal.sna_f Internal.sf_fstate
getFmul :: SnoptA [SnDoubleReal]
getFmul   = getArray Internal.sf_nF Internal.sna_f Internal.sf_fmul
getF :: SnoptA [SnDoubleReal]
getF      = getArray Internal.sf_nF Internal.sna_f Internal.sf_f
getObjRow :: SnoptA SnInteger
getObjRow = getScalar Internal.sna_f Internal.sf_objRow
getObjAdd :: SnoptA SnDoubleReal
getObjAdd = getScalar Internal.sna_f Internal.sf_objAdd

setFlow :: [SnDoubleReal] -> SnoptA ()
setFlow   = setArray "flow"   Internal.sf_nF Internal.sna_f Internal.sf_flow
setFupp :: [SnDoubleReal] -> SnoptA ()
setFupp   = setArray "fupp"   Internal.sf_nF Internal.sna_f Internal.sf_fupp
setFstate :: [SnInteger] -> SnoptA ()
setFstate = setArray "fstate" Internal.sf_nF Internal.sna_f Internal.sf_fstate
setFmul :: [SnDoubleReal] -> SnoptA ()
setFmul   = setArray "fmul"   Internal.sf_nF Internal.sna_f Internal.sf_fmul
setF :: [SnDoubleReal] -> SnoptA ()
setF      = setArray "f"      Internal.sf_nF Internal.sna_f Internal.sf_f
setObjRow :: SnInteger -> SnoptA ()
setObjRow = setScalar Internal.sna_f Internal.sf_objRow
setObjAdd :: SnDoubleReal -> SnoptA ()
setObjAdd = setScalar Internal.sna_f Internal.sf_objAdd

getIAfun :: SnoptA [SnInteger]
getIAfun = getArray  Internal.sa_neA Internal.sna_a  Internal.sa_iAfun
getJAvar :: SnoptA [SnInteger]
getJAvar = getArray  Internal.sa_neA Internal.sna_a  Internal.sa_jAvar
getA :: SnoptA [SnDoubleReal]
getA     = getArray  Internal.sa_neA Internal.sna_a  Internal.sa_a

setIAfun :: [SnInteger] -> SnoptA ()
setIAfun = setArray "iAfun"  Internal.sa_neA Internal.sna_a  Internal.sa_iAfun
setJAvar :: [SnInteger] -> SnoptA ()
setJAvar = setArray "jAvar"  Internal.sa_neA Internal.sna_a  Internal.sa_jAvar
setA :: [SnDoubleReal] -> SnoptA ()
setA     = setArray "A"      Internal.sa_neA Internal.sna_a  Internal.sa_a

getIGfun :: SnoptA [SnInteger]
getIGfun = getArray  Internal.sg_neG Internal.sna_g  Internal.sg_iGfun
getJGvar :: SnoptA [SnInteger]
getJGvar = getArray  Internal.sg_neG Internal.sna_g  Internal.sg_jGvar

setIGfun :: [SnInteger] -> SnoptA ()
setIGfun = setArray "iGfun"  Internal.sg_neG Internal.sna_g  Internal.sg_iGfun
setJGvar :: [SnInteger] -> SnoptA ()
setJGvar = setArray "jGvar"  Internal.sg_neG Internal.sna_g  Internal.sg_jGvar

getScalar' :: Storable a => Getting (Ptr a) SnoptA' (Ptr a) -> SnoptA a
getScalar' getXFAG = do
  snoptA <- ask
  liftIO $ peek (snoptA ^. getXFAG)

setScalar' :: Storable a => Getting (Ptr a) SnoptA' (Ptr a) -> a -> SnoptA ()
setScalar' getXFAG val = do
  snoptA <- ask
  liftIO $ poke (snoptA ^. getXFAG) val

getIprint :: SnoptA SnInteger
getIprint = getScalar' Internal.sna_iprint

setIprint :: SnInteger -> SnoptA ()
setIprint = setScalar' Internal.sna_iprint

getIsummary :: SnoptA SnInteger
getIsummary = getScalar' Internal.sna_isummary

setIsummary :: SnInteger -> SnoptA ()
setIsummary = setScalar' Internal.sna_isummary

--------------------------------------------------------------------------------------
sninit :: SnoptA ()
sninit = do
  sna <- ask
  liftIO $ Internal.sninit sna

snseti :: String -> SnInteger -> SnoptA SnInteger
snseti name val = do
  sna' <- ask
  liftIO $ Internal.snseti sna' name val

snjac :: SnoptA (SnInteger,SnInteger,SnInteger,SnInteger)
snjac = do
  sna <- ask
  liftIO $ Internal.snjac sna

snopta :: String -> SnoptA SnInteger
snopta name = do
  x <- ask
  liftIO $ Internal.snopta x name

toy1 :: IO ()
toy1 = do
  let x = [ (1, (    0, 1e12))
          , (1, (-1e12, 1e12))
          ]
      f = [ (-1e12, 1e12)
          , (-1e12, 4)
          , (-1e12, 5)
          ]
      a = []
      g = [ (1,1)
          , (1,2)
          , (2,1)
          , (2,2)
          , (3,1)
          , (3,2)
          ]

      nx = length x
      nf = length f
      na = length a
      ng = length g

      xlow = map (fst . snd) x
      xupp = map (snd . snd) x
      xInit = map fst x

      (flow,fupp) = unzip f
      f0 = [0,0,0]

      (iA,jA,aval) = unzip3 a
      (iG,jG) = unzip g

  let userfg _ _ x' needF _ f' needG _ g' _ _ _ _ _ _ = do
        needF' <- peek needF
        needG' <- peek needG
        unless (needF' `elem` [0,1]) $ error "needF isn't 1 or 0"
        unless (needG' `elem` [0,1]) $ error "needG isn't 1 or 0"
        when (needF' == 1) $ do
          x0 <- peekElemOff x' 0
          x1 <- peekElemOff x' 1
          pokeElemOff f' 0 (x1)
          pokeElemOff f' 1 (x0*x0 + 4*x1*x1)
          pokeElemOff f' 2 ((x0 - 2)*(x0 - 2) + x1*x1)

        when (needG' == 1) $ do
          x0 <- peekElemOff x' 0
          x1 <- peekElemOff x' 1
          pokeElemOff g' 0 0
          pokeElemOff g' 1 1
          pokeElemOff g' 2 (2*x0)
          pokeElemOff g' 3 (8*x1)
          pokeElemOff g' 4 (2*(x0-1))
          pokeElemOff g' 5 (2*x1)

  ret <- runSnoptA 500 10000 20000 nx nf na ng userfg $ do
    sninit

    setXlow xlow
    setXupp xupp
    setX xInit

    setFlow flow
    setFupp fupp
    setF f0

    setObjRow 1
    setObjAdd 0

    setIAfun iA
    setJAvar jA
    setA aval

    setIGfun iG
    setJGvar jG
    snopta "toy1"
  putStrLn $ "snopta: ret: " ++ show ret
