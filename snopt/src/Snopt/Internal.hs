{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}

module Snopt.Internal ( -- * types
                        SnoptA'
                      , newSnoptA
                        -- * the functions
                      , sninit
                      , snseti
                      , snjac
                      , snopta
                        -- * extra
                      , wrap
                        -- * lenses
                      , sx_nx
                      , sx_xlow
                      , sx_xupp
                      , sx_x
                      , sx_xstate
                      , sx_xmul

                      , sf_nF
                      , sf_flow
                      , sf_fupp
                      , sf_fstate
                      , sf_fmul
                      , sf_f
                      , sf_objRow
                      , sf_objAdd

                      , sa_iAfun
                      , sa_jAvar
                      , sa_a
                      , sa_lenA
                      , sa_neA

                      , sg_iGfun
                      , sg_jGvar
                      , sg_lenG
                      , sg_neG

                      , sm_info
                      , sm_mincw
                      , sm_miniw
                      , sm_minrw
                      , sm_iprint
                      , sm_isummary
                      , sm_start
                      , sm_ns
                      , sm_ninf
                      , sm_sinf
                      , sm_errors

                      , sna_ws
                      , sna_x
                      , sna_f
                      , sna_a
                      , sna_g
                      , sna_m
                      , sna_ufp
                      ) where

import Control.Lens ( makeLenses )
import Control.Lens.Getter
import qualified Data.Vector.Storable.Mutable as SVM
import Foreign.C.String
import Foreign.Ptr ( FunPtr, Ptr )
import Foreign.Marshal
import Foreign.Storable

import Snopt.FFI

type Vec = SVM.IOVector

data Workspace f = Workspace
                   (f SnChar) (f SnInteger) (f SnDoubleReal)
                   (f SnInteger) (f SnInteger) (f SnInteger)

data SnX f = SnX { _sx_nx     :: f SnInteger
                 , _sx_xlow   :: f SnDoubleReal
                 , _sx_xupp   :: f SnDoubleReal
                 , _sx_x      :: f SnDoubleReal
                 , _sx_xstate :: f SnInteger
                 , _sx_xmul   :: f SnDoubleReal
                 }
data SnF f = SnF { _sf_nF :: f SnInteger
                 , _sf_flow :: f SnDoubleReal
                 , _sf_fupp :: f SnDoubleReal
                 , _sf_fstate :: f SnInteger
                 , _sf_fmul :: f SnDoubleReal
                 , _sf_f :: f SnDoubleReal
                 , _sf_objRow :: f SnInteger
                 , _sf_objAdd :: f SnDoubleReal
               }
data SnA f = SnA { _sa_iAfun :: f SnInteger
                 , _sa_jAvar :: f SnInteger
                 , _sa_a :: f SnDoubleReal
                 , _sa_lenA :: f SnInteger -- size of vector
                 , _sa_neA :: f SnInteger -- number of elements in vector
                 }
data SnG f = SnG { _sg_iGfun :: f SnInteger
                 , _sg_jGvar :: f SnInteger
                 , _sg_lenG :: f SnInteger -- size of vector
                 , _sg_neG :: f SnInteger -- number of elements in vector
                 }
data SnM f = SnM { _sm_info :: f SnInteger
                 , _sm_mincw :: f SnInteger
                 , _sm_miniw :: f SnInteger
                 , _sm_minrw :: f SnInteger
                 , _sm_iprint :: f SnInteger
                 , _sm_isummary :: f SnInteger
                 , _sm_start :: f SnInteger
                 , _sm_ns :: f SnInteger
                 , _sm_ninf :: f SnInteger
                 , _sm_sinf :: f SnDoubleReal
                 , _sm_errors :: f SnInteger
                 }
data SnoptA' f = SnoptA' { _sna_ws :: Workspace f
                         , _sna_x :: SnX f
                         , _sna_f :: SnF f
                         , _sna_a :: SnA f
                         , _sna_g :: SnG f
                         , _sna_m :: SnM f
                         , _sna_ufp :: FunPtr U_fp
                         }
makeLenses ''SnX
makeLenses ''SnF
makeLenses ''SnA
makeLenses ''SnG
makeLenses ''SnM
makeLenses ''SnoptA'

newSnoptA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> FunPtr U_fp -> IO (SnoptA' Vec)
newSnoptA nc ni nr nx nf na ng ufp = do
  (sa,sg) <- newAG na ng
  workspace <- newWorkspace nc ni nr
  sx <- newX nx
  sf <- newF nf
  sm <- newM
  return $ SnoptA' workspace sx sf sa sg sm ufp

svmNewSet :: (Storable a) => Int -> a -> IO (SVM.IOVector a)
svmNewSet n val = do
  v <- SVM.new n
  SVM.set v val
  return v

newWorkspace :: Int -> Int -> Int -> IO (Workspace Vec)
newWorkspace lencw leniw lenrw = do
  ncw <- svmNewSet 1 (fromIntegral lencw)
  niw <- svmNewSet 1 (fromIntegral leniw)
  nrw <- svmNewSet 1 (fromIntegral lenrw)
  cw <- svmNewSet (8*lencw) 0
  iw <- svmNewSet leniw     0
  rw <- svmNewSet lenrw     0
  return $ Workspace cw iw rw ncw niw nrw

newX :: Int -> IO (SnX Vec)
newX n = do
  nx     <- svmNewSet 1 (fromIntegral n)
  x      <- svmNewSet n 0
  xlow   <- svmNewSet n 0
  xupp   <- svmNewSet n 0
  xstate <- svmNewSet n 0
  xmul   <- svmNewSet n 0
  return SnX { _sx_nx = nx
             , _sx_xlow = xlow
             , _sx_xupp = xupp
             , _sx_x = x
             , _sx_xstate = xstate
             , _sx_xmul = xmul
             }

newF :: Int -> IO (SnF Vec)
newF n = do
  nf  <- svmNewSet 1 (fromIntegral n)
  f      <- svmNewSet n 0
  flow   <- svmNewSet n 0
  fupp   <- svmNewSet n 0
  fstate <- svmNewSet n 0
  fmul   <- svmNewSet n 0
  objrow <- svmNewSet 1 1
  objadd <- svmNewSet 1 0

  return SnF { _sf_nF = nf
             , _sf_flow = flow
             , _sf_fupp = fupp
             , _sf_fstate = fstate
             , _sf_fmul = fmul
             , _sf_f = f
             , _sf_objRow = objrow
             , _sf_objAdd = objadd
             }

newAG :: Int -> Int -> IO (SnA Vec, SnG Vec)
newAG neA neG = do
  let lenA | neA < 1 = 1
           | otherwise = neA
      lenG | neG < 1 = 1
           | otherwise = neG

  neA'  <- svmNewSet 1 (fromIntegral neA)
  lenA' <- svmNewSet 1 (fromIntegral lenA)
  iA'   <- svmNewSet lenA 0
  jA'   <- svmNewSet lenA 0
  a'    <- svmNewSet lenA 0

  neG'  <- svmNewSet 1 (fromIntegral neG)
  lenG' <- svmNewSet 1 (fromIntegral lenG)
  iG' <- svmNewSet lenG 0
  jG' <- svmNewSet lenG 0

  let sna = SnA { _sa_iAfun = iA'
                , _sa_jAvar = jA'
                , _sa_neA = neA'
                , _sa_lenA = lenA'
                , _sa_a = a'
                }
      sng = SnG { _sg_iGfun = iG'
                , _sg_jGvar = jG'
                , _sg_neG = neG'
                , _sg_lenG = lenG'
                }
  return (sna, sng)

newM :: IO (SnM Vec)
newM = do
  info <- svmNewSet 1 0
  mincw <- svmNewSet 1 0
  miniw <- svmNewSet 1 0
  minrw <- svmNewSet 1 0
  iprint   <- svmNewSet 1 0
  isummary <- svmNewSet 1 6
  start  <- svmNewSet 1 0
  ns  <- svmNewSet 1 0
  ninf  <- svmNewSet 1 0
  sinf  <- svmNewSet 1 0
  errors <- svmNewSet 1 0
  return SnM { _sm_info = info
             , _sm_mincw = mincw
             , _sm_miniw = miniw
             , _sm_minrw = minrw
             , _sm_iprint = iprint
             , _sm_isummary = isummary
             , _sm_start = start
             , _sm_ns = ns
             , _sm_ninf = ninf
             , _sm_sinf = sinf
             , _sm_errors = errors
             }


--------------------------------------------------------------------------------------
sninit' :: Workspace Vec -> SnM Vec -> IO ()
sninit' ws misc =
  unsafeWithW ws $ \(Workspace cw iw rw lencw leniw lenrw) ->
  unsafeWithM misc $ \(SnM {_sm_iprint = iprint, _sm_isummary = isummary }) -> do
    cw_len <- peek lencw
    c_sninit_ iprint isummary cw lencw iw leniw rw lenrw (cw_len*8)

sninit :: SnoptA' Vec -> IO ()
sninit sna = sninit' (sna ^. sna_ws ) (sna ^. sna_m)

snseti' :: Workspace Vec -> SnM Vec -> String -> SnInteger -> IO SnInteger
snseti' ws misc name value =
  unsafeWithW ws $ \(Workspace cw iw rw lencw leniw lenrw) ->
  unsafeWithM misc $ \(SnM {_sm_iprint = iprint, _sm_isummary = isummary}) -> do
    errors' <- new 0
    (name',bufferlen) <- newCStringLen name
    value' <- new value

    cw_len <- peek lencw
    c_snseti_
      name' value' iprint isummary errors'
      cw lencw iw leniw rw lenrw
      (fromIntegral bufferlen) (8*cw_len)
    errors <- peek errors'

    free errors'
    free name'
    free value'

    return errors

snseti :: SnoptA' Vec -> String -> SnInteger -> IO SnInteger
snseti sna = snseti' (sna ^. sna_ws) (sna ^. sna_m)

unsafeWithX :: SnX Vec -> (SnX Ptr -> IO a) -> IO a
unsafeWithX (SnX x0 x1 x2 x3 x4 x5) f =
  SVM.unsafeWith x0 $ \x0' ->
  SVM.unsafeWith x1 $ \x1' ->
  SVM.unsafeWith x2 $ \x2' ->
  SVM.unsafeWith x3 $ \x3' ->
  SVM.unsafeWith x4 $ \x4' ->
  SVM.unsafeWith x5 $ \x5' ->
  f (SnX x0' x1' x2' x3' x4' x5')

unsafeWithF :: SnF Vec -> (SnF Ptr -> IO a) -> IO a
unsafeWithF (SnF x0 x1 x2 x3 x4 x5 x6 x7) f =
  SVM.unsafeWith x0 $ \x0' ->
  SVM.unsafeWith x1 $ \x1' ->
  SVM.unsafeWith x2 $ \x2' ->
  SVM.unsafeWith x3 $ \x3' ->
  SVM.unsafeWith x4 $ \x4' ->
  SVM.unsafeWith x5 $ \x5' ->
  SVM.unsafeWith x6 $ \x6' ->
  SVM.unsafeWith x7 $ \x7' ->
  f (SnF x0' x1' x2' x3' x4' x5' x6' x7')

unsafeWithA :: SnA Vec -> (SnA Ptr -> IO a) -> IO a
unsafeWithA (SnA x0 x1 x2 x3 x4) f =
  SVM.unsafeWith x0 $ \x0' ->
  SVM.unsafeWith x1 $ \x1' ->
  SVM.unsafeWith x2 $ \x2' ->
  SVM.unsafeWith x3 $ \x3' ->
  SVM.unsafeWith x4 $ \x4' ->
  f (SnA x0' x1' x2' x3' x4')

unsafeWithG :: SnG Vec -> (SnG Ptr -> IO a) -> IO a
unsafeWithG (SnG x0 x1 x2 x3) f =
  SVM.unsafeWith x0 $ \x0' ->
  SVM.unsafeWith x1 $ \x1' ->
  SVM.unsafeWith x2 $ \x2' ->
  SVM.unsafeWith x3 $ \x3' ->
  f (SnG x0' x1' x2' x3')

unsafeWithW :: Workspace Vec -> (Workspace Ptr -> IO a) -> IO a
unsafeWithW (Workspace x0 x1 x2 x3 x4 x5) f =
  SVM.unsafeWith x0 $ \x0' ->
  SVM.unsafeWith x1 $ \x1' ->
  SVM.unsafeWith x2 $ \x2' ->
  SVM.unsafeWith x3 $ \x3' ->
  SVM.unsafeWith x4 $ \x4' ->
  SVM.unsafeWith x5 $ \x5' ->
  f (Workspace x0' x1' x2' x3' x4' x5')

unsafeWithM :: SnM Vec -> (SnM Ptr -> IO a) -> IO a
unsafeWithM (SnM x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) f =
  SVM.unsafeWith x0 $ \x0' ->
  SVM.unsafeWith x1 $ \x1' ->
  SVM.unsafeWith x2 $ \x2' ->
  SVM.unsafeWith x3 $ \x3' ->
  SVM.unsafeWith x4 $ \x4' ->
  SVM.unsafeWith x5 $ \x5' ->
  SVM.unsafeWith x6 $ \x6' ->
  SVM.unsafeWith x7 $ \x7' ->
  SVM.unsafeWith x8 $ \x8' ->
  SVM.unsafeWith x9 $ \x9' ->
  SVM.unsafeWith x10 $ \x10' ->
  f (SnM x0' x1' x2' x3' x4' x5' x6' x7' x8' x9' x10')

unsafeWithSnoptA' :: SnoptA' Vec -> (SnoptA' Ptr -> IO a) -> IO a
unsafeWithSnoptA' (SnoptA' x0 x1 x2 x3 x4 x5 x6) f =
  unsafeWithW x0 $ \x0' ->
  unsafeWithX x1 $ \x1' ->
  unsafeWithF x2 $ \x2' ->
  unsafeWithA x3 $ \x3' ->
  unsafeWithG x4 $ \x4' ->
  unsafeWithM x5 $ \x5' ->
  f (SnoptA' x0' x1' x2' x3' x4' x5' x6)

snjac :: SnoptA' Vec -> IO ()
snjac = flip unsafeWithSnoptA' snjac'

snjac' :: SnoptA' Ptr -> IO ()
snjac' (SnoptA' workspace
        (SnX { _sx_x = x, _sx_xlow = xlow, _sx_xupp = xupp, _sx_nx = nx })
        (SnF { _sf_nF = nF })
        (SnA { _sa_iAfun = iAfun, _sa_jAvar = jAvar, _sa_lenA = lenA, _sa_neA = neA, _sa_a = a })
        (SnG { _sg_iGfun = iGfun, _sg_jGvar = jGvar, _sg_lenG = lenG, _sg_neG = neG })
        (SnM { _sm_info = info, _sm_mincw = mincw, _sm_miniw = miniw, _sm_minrw = minrw})
        userf ) = do
  let Workspace cu iu ru lencu leniu lenru = workspace
      Workspace cw iw rw lencw leniw lenrw = workspace

  cu_len <- peek lencu
  cw_len <- peek lencw

  c_snjac_
    info nF nx userf
    iAfun jAvar lenA neA a
    iGfun jGvar lenG neG
    x xlow xupp mincw miniw minrw
    cu lencu iu leniu ru lenru
    cw lencw iw leniw rw lenrw
    (8*cu_len) (8*cw_len)

snopta :: SnoptA' Vec -> String -> IO ()
snopta sna = unsafeWithSnoptA' sna . flip snopta'

snopta' :: SnoptA' Ptr -> String -> IO ()
snopta' (SnoptA' workspace
  (SnX { _sx_x = x, _sx_xlow = xlow, _sx_xupp = xupp, _sx_nx = nx, _sx_xstate = xstate, _sx_xmul = xmul})
  (SnF { _sf_nF = nF, _sf_fstate = fstate, _sf_objAdd = objAdd, _sf_objRow = objRow
       , _sf_flow = flow, _sf_fupp = fupp, _sf_f = f, _sf_fmul = fmul })
  (SnA { _sa_iAfun = iAfun, _sa_jAvar = jAvar, _sa_lenA = lenA, _sa_neA = neA, _sa_a = a})
  (SnG { _sg_iGfun = iGfun, _sg_jGvar = jGvar, _sg_lenG = lenG, _sg_neG = neG})
  (SnM { _sm_info = info, _sm_mincw = mincw, _sm_miniw = miniw, _sm_minrw = minrw
       , _sm_start = start, _sm_ns = ns, _sm_ninf = ninf, _sm_sinf = sinf })
  userf ) problemName
  = do
    let Workspace cu iu ru lencu leniu lenru = workspace
        Workspace cw iw rw lencw leniw lenrw = workspace
    let nxname' = 1
        nfname' = 1

    nxname <- new nxname'
    nfname <- new nfname'

    xnames <- mallocArray (8 * fromIntegral nxname')
    fnames <- mallocArray (8 * fromIntegral nfname')

    (probstr,prob_len) <- newCStringLen (problemName ++ "        ")

    cu_len' <- peek lencu
    cw_len' <- peek lencw

    c_snopta_ start nF nx nxname nfname
      objAdd objRow probstr userf
      iAfun jAvar lenA neA a
      iGfun jGvar lenG neG
      xlow xupp xnames flow fupp fnames
      x xstate xmul f fstate fmul
      info mincw miniw minrw
      ns ninf sinf
      cu lencu iu leniu ru lenru
      cw lencw iw leniw rw lenrw
      (fromIntegral prob_len) (8*nxname') (8*nfname') (8*cu_len') (8*cw_len')

    free nxname
    free nfname
    free xnames
    free fnames
    free probstr
