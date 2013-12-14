{-# OPTIONS_GHC -Wall #-}
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

                    , getInfo
                    , getMincw
                    , getMiniw
                    , getMinrw

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

                    , setStart
                    , getStart
                    , getNs
                    , getNinf
                    , getSinf
                    , getErrors

                      -- * example
                    , toy1
                    ) where

import Control.Lens ( Getting, Accessor, (^.) )
import Control.Monad ( unless, when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT, throwError )
import Control.Monad.Reader ( ReaderT, MonadReader, runReaderT, ask )
import Control.Monad.IO.Class
import Foreign.Storable

import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SVM

import Snopt.FFI
import qualified Snopt.Internal as Internal
import Snopt.Internal ( SnoptA', newSnoptA )

newtype SnoptA a = SnoptA { toSnoptA :: ErrorT String (ReaderT (SnoptA' SVM.IOVector) IO) a }
                 deriving ( Monad
                          , MonadIO
                          , MonadError String
                          , MonadReader (SnoptA' SVM.IOVector)
                          )
runSnoptA :: Int -> Int -> Int -> Int -> Int -> Int -> Int
          -> U_fp -> SnoptA a -> IO (Either String a)
runSnoptA nc ni nr nx nf na ng ufp userSnoptA = do
  ufp' <- wrap ufp
  sna <- newSnoptA nc ni nr nx nf na ng ufp'
  flip runReaderT sna . runErrorT $ toSnoptA userSnoptA

--------------------------- setters/getters ----------------------------
getArray :: Storable a =>
            ((xfag -> Accessor xfag xfag) -> SnoptA' SVM.IOVector -> Accessor xfag (SnoptA' SVM.IOVector))
         -> Getting (SVM.IOVector a) xfag (SVM.IOVector a)
         -> SnoptA (SV.Vector a)
getArray getXFAG getField = do
  snoptA <- ask
  let v = (snoptA ^. getXFAG) ^. getField
  liftIO $ SVM.clone v >>= SV.freeze

setArray :: Storable a =>
            String
         -> ((xfag -> Accessor xfag xfag) -> SnoptA' SVM.IOVector -> Accessor xfag (SnoptA' SVM.IOVector))
         -> Getting (SVM.IOVector a) xfag (SVM.IOVector a)
         -> SV.Vector a
         -> SnoptA ()
setArray name getXFAG getField userVec = do
  snoptA <- ask
  let vec = snoptA ^. getXFAG ^. getField
      trueLen = SVM.length vec
      userLen = SV.length userVec
  when (userLen /= trueLen) $
    throwError $ "setArray dimension mismatch: " ++ name ++ ": user length " ++ show userLen ++ " /= " ++ show trueLen
  liftIO $ SV.copy vec userVec

-- these are vectors which must be length > 0
getArray' :: Storable a =>
             Getting (SVM.IOVector SnInteger) xfag (SVM.IOVector SnInteger)
          -> ((xfag -> Accessor xfag xfag) -> SnoptA' SVM.IOVector -> Accessor xfag (SnoptA' SVM.IOVector))
          -> Getting (SVM.IOVector a) xfag (SVM.IOVector a)
          -> SnoptA (SV.Vector a)
getArray' getN getXFAG getField = do
  snoptA <- ask
  let v = (snoptA ^. getXFAG) ^. getField
  n <- liftIO $ SVM.read ((snoptA ^. getXFAG) ^. getN) 0
  v' <- liftIO $ SVM.clone v >>= SV.freeze
  return (SV.take (fromIntegral n) v')

setArray' :: Storable a =>
             String
          -> Getting (SVM.IOVector SnInteger) xfag (SVM.IOVector SnInteger)
          -> ((xfag -> Accessor xfag xfag) -> SnoptA' SVM.IOVector -> Accessor xfag (SnoptA' SVM.IOVector))
          -> Getting (SVM.IOVector a) xfag (SVM.IOVector a)
          -> (SV.Vector a)
          -> SnoptA ()
setArray' name getN getXFAG getField userVec = do
  snoptA <- ask
  n <- liftIO $ SVM.read ((snoptA ^. getXFAG) ^. getN) 0
  let vec = snoptA ^. getXFAG ^. getField
      trueLen = SVM.length vec
      userLen = SV.length userVec
  case userLen of
    0 -> do
      when (n /= 0) $
        throwError $ "setArray dimension mismatch: " ++ name ++ ": user length " ++ show userLen ++ " /= " ++ show n
    _ -> do
      when (userLen /= trueLen) $
        throwError $ "setArray dimension mismatch: " ++ name ++ ": user length " ++ show userLen ++ " /= " ++ show trueLen
      liftIO $ SV.copy vec userVec

getScalar :: Storable a =>
             ((xfag -> Accessor xfag xfag) -> SnoptA' SVM.IOVector -> Accessor xfag (SnoptA' SVM.IOVector))
          -> Getting (SVM.IOVector a) xfag (SVM.IOVector a)
          -> SnoptA a
getScalar getXFAG getField = do
  snoptA <- ask
  liftIO $ SVM.read ((snoptA ^. getXFAG) ^. getField) 0

setScalar :: Storable a =>
             ((xfag -> Accessor xfag xfag) -> SnoptA' SVM.IOVector -> Accessor xfag (SnoptA' SVM.IOVector))
          -> Getting (SVM.IOVector a) xfag (SVM.IOVector a)
          -> a
          -> SnoptA ()
setScalar getXFAG getField val = do
  snoptA <- ask
  liftIO $ SVM.write ((snoptA ^. getXFAG) ^. getField) 0 val

getXlow :: SnoptA (SV.Vector SnDoubleReal)
getXlow   = getArray Internal.sna_x Internal.sx_xlow
getXupp :: SnoptA (SV.Vector SnDoubleReal)
getXupp   = getArray Internal.sna_x Internal.sx_xupp
getX :: SnoptA (SV.Vector SnDoubleReal)
getX      = getArray Internal.sna_x Internal.sx_x
getXstate :: SnoptA (SV.Vector SnInteger)
getXstate = getArray Internal.sna_x Internal.sx_xstate
getXmul :: SnoptA (SV.Vector SnDoubleReal)
getXmul   = getArray Internal.sna_x Internal.sx_xmul

setXlow :: (SV.Vector SnDoubleReal) -> SnoptA ()
setXlow   = setArray "xlow"   Internal.sna_x Internal.sx_xlow
setXupp :: (SV.Vector SnDoubleReal) -> SnoptA ()
setXupp   = setArray "xupp"   Internal.sna_x Internal.sx_xupp
setX :: (SV.Vector SnDoubleReal) -> SnoptA ()
setX      = setArray "x"      Internal.sna_x Internal.sx_x
setXstate :: (SV.Vector SnInteger) -> SnoptA ()
setXstate = setArray "xstate" Internal.sna_x Internal.sx_xstate
setXmul :: (SV.Vector SnDoubleReal) -> SnoptA ()
setXmul   = setArray "xmul  " Internal.sna_x Internal.sx_xmul

getFlow :: SnoptA (SV.Vector SnDoubleReal)
getFlow   = getArray Internal.sna_f Internal.sf_flow
getFupp :: SnoptA (SV.Vector SnDoubleReal)
getFupp   = getArray Internal.sna_f Internal.sf_fupp
getFstate :: SnoptA (SV.Vector SnInteger)
getFstate = getArray Internal.sna_f Internal.sf_fstate
getFmul :: SnoptA (SV.Vector SnDoubleReal)
getFmul   = getArray Internal.sna_f Internal.sf_fmul
getF :: SnoptA (SV.Vector SnDoubleReal)
getF      = getArray Internal.sna_f Internal.sf_f
getObjRow :: SnoptA SnInteger
getObjRow = getScalar Internal.sna_f Internal.sf_objRow
getObjAdd :: SnoptA SnDoubleReal
getObjAdd = getScalar Internal.sna_f Internal.sf_objAdd

setFlow :: (SV.Vector SnDoubleReal) -> SnoptA ()
setFlow   = setArray "flow"   Internal.sna_f Internal.sf_flow
setFupp :: (SV.Vector SnDoubleReal) -> SnoptA ()
setFupp   = setArray "fupp"   Internal.sna_f Internal.sf_fupp
setFstate :: (SV.Vector SnInteger) -> SnoptA ()
setFstate = setArray "fstate" Internal.sna_f Internal.sf_fstate
setFmul :: (SV.Vector SnDoubleReal) -> SnoptA ()
setFmul   = setArray "fmul"   Internal.sna_f Internal.sf_fmul
setF :: (SV.Vector SnDoubleReal) -> SnoptA ()
setF      = setArray "f"      Internal.sna_f Internal.sf_f
setObjRow :: SnInteger -> SnoptA ()
setObjRow = setScalar Internal.sna_f Internal.sf_objRow
setObjAdd :: SnDoubleReal -> SnoptA ()
setObjAdd = setScalar Internal.sna_f Internal.sf_objAdd

getIAfun :: SnoptA (SV.Vector SnInteger)
getIAfun = getArray'  Internal.sa_neA Internal.sna_a  Internal.sa_iAfun
getJAvar :: SnoptA (SV.Vector SnInteger)
getJAvar = getArray'  Internal.sa_neA Internal.sna_a  Internal.sa_jAvar
getA :: SnoptA (SV.Vector SnDoubleReal)
getA     = getArray'  Internal.sa_neA Internal.sna_a  Internal.sa_a

setIAfun :: (SV.Vector SnInteger) -> SnoptA ()
setIAfun = setArray' "iAfun"  Internal.sa_neA Internal.sna_a  Internal.sa_iAfun
setJAvar :: (SV.Vector SnInteger) -> SnoptA ()
setJAvar = setArray' "jAvar"  Internal.sa_neA Internal.sna_a  Internal.sa_jAvar
setA :: (SV.Vector SnDoubleReal) -> SnoptA ()
setA     = setArray' "A"      Internal.sa_neA Internal.sna_a  Internal.sa_a

getIGfun :: SnoptA (SV.Vector SnInteger)
getIGfun = getArray'  Internal.sg_neG Internal.sna_g  Internal.sg_iGfun
getJGvar :: SnoptA (SV.Vector SnInteger)
getJGvar = getArray'  Internal.sg_neG Internal.sna_g  Internal.sg_jGvar

setIGfun :: (SV.Vector SnInteger) -> SnoptA ()
setIGfun = setArray' "iGfun"  Internal.sg_neG Internal.sna_g  Internal.sg_iGfun
setJGvar :: (SV.Vector SnInteger) -> SnoptA ()
setJGvar = setArray' "jGvar"  Internal.sg_neG Internal.sna_g  Internal.sg_jGvar

getInfo :: SnoptA SnInteger
getInfo = getScalar Internal.sna_m Internal.sm_info

getMincw :: SnoptA SnInteger
getMincw = getScalar Internal.sna_m Internal.sm_mincw

getMiniw :: SnoptA SnInteger
getMiniw = getScalar Internal.sna_m Internal.sm_miniw

getMinrw :: SnoptA SnInteger
getMinrw = getScalar Internal.sna_m Internal.sm_minrw

getIprint :: SnoptA SnInteger
getIprint = getScalar Internal.sna_m Internal.sm_iprint

setIprint :: SnInteger -> SnoptA ()
setIprint = setScalar Internal.sna_m Internal.sm_iprint

getIsummary :: SnoptA SnInteger
getIsummary = getScalar Internal.sna_m Internal.sm_isummary

setIsummary :: SnInteger -> SnoptA ()
setIsummary = setScalar Internal.sna_m Internal.sm_isummary

setStart :: SnInteger -> SnoptA ()
setStart = setScalar Internal.sna_m Internal.sm_start

getStart :: SnoptA SnInteger
getStart = getScalar Internal.sna_m Internal.sm_start

getNs :: SnoptA SnInteger
getNs = getScalar Internal.sna_m Internal.sm_ns

getNinf :: SnoptA SnInteger
getNinf = getScalar Internal.sna_m Internal.sm_ninf

getSinf :: SnoptA SnDoubleReal
getSinf = getScalar Internal.sna_m Internal.sm_sinf

getErrors :: SnoptA SnInteger
getErrors = getScalar Internal.sna_m Internal.sm_errors

--------------------------------------------------------------------------------------
sninit :: SnoptA ()
sninit = do
  sna <- ask
  liftIO $ Internal.sninit sna

snseti :: String -> SnInteger -> SnoptA SnInteger
snseti name val = do
  sna' <- ask
  liftIO $ Internal.snseti sna' name val

snjac :: SnoptA ()
snjac = do
  sna <- ask
  liftIO $ Internal.snjac sna

snopta :: String -> SnoptA ()
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
          pokeElemOff f' 0 x1
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

    setXlow $ SV.fromList xlow
    setXupp $ SV.fromList xupp
    setX $ SV.fromList xInit

    setFlow $ SV.fromList flow
    setFupp $ SV.fromList fupp
    setF $ SV.fromList f0

    setObjRow 1
    setObjAdd 0

    setIAfun $ SV.fromList iA
    setJAvar $ SV.fromList jA
    setA $ SV.fromList aval

    setIGfun $ SV.fromList iG
    setJGvar $ SV.fromList jG
    snopta "toy1"
  putStrLn $ "snopta: " ++ show ret
