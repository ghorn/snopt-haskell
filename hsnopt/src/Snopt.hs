{-# OPTIONS_GHC -Wall #-}

--module Snopt ( Workspace(..)
--             , mallocWorkspace
--             , freeWorkspace
--             ) where

import Control.Monad ( when )
import Foreign.C.String
import Foreign.Ptr ( FunPtr, Ptr )
import Foreign.Marshal
import Foreign.Storable
--import Foreign.C.Types ( CDouble(..), CInt(..) )

import Snopt.Bindings

global_printval :: SnInteger
global_printval = 0

global_summaryval :: SnInteger
global_summaryval = 6

data Workspace = Workspace { wsCw :: Ptr SnChar
                           , wsIw :: Ptr SnInteger
                           , wsRw :: Ptr SnDoubleReal
                           , wsNcw :: Ptr SnInteger
                           , wsNiw :: Ptr SnInteger
                           , wsNrw :: Ptr SnInteger
                           }
data SnX = SnX { sx_nx :: Ptr SnInteger
               , sx_xlow :: Ptr SnDoubleReal
               , sx_xupp :: Ptr SnDoubleReal
               , sx_x :: Ptr SnDoubleReal
               , sx_xstate :: Ptr SnInteger
               , sx_xmul :: Ptr SnDoubleReal
               }
data SnF = SnF { sf_nF :: Ptr SnInteger
               , sf_flow :: Ptr SnDoubleReal
               , sf_fupp :: Ptr SnDoubleReal
               , sf_fstate :: Ptr SnInteger
               , sf_fmul :: Ptr SnDoubleReal
               , sf_f :: Ptr SnDoubleReal
               , sf_userf :: FunPtr U_fp
               , sf_objRow :: Ptr SnInteger
               , sf_objAdd :: Ptr SnDoubleReal
               }
data SnA = SnA { sa_iAfun :: Ptr SnInteger
               , sa_jAvar :: Ptr SnInteger
               , sa_a :: Ptr SnDoubleReal
               , sa_lenA :: Ptr SnInteger -- size of vector
               , sa_neA :: Ptr SnInteger -- number of elements in vector
               }
data SnG = SnG { sg_iGfun :: Ptr SnInteger
               , sg_jGvar :: Ptr SnInteger
               , sg_lenG :: Ptr SnInteger -- size of vector
               , sg_neG :: Ptr SnInteger -- number of elements in vector
               }

setOptionI :: Workspace -> String -> SnInteger -> IO ()
setOptionI (Workspace cw iw rw lencw leniw lenrw) name value = do
  iPrt' <- new global_printval
  iSum' <- new global_summaryval
  errors' <- new 0
  (name',bufferlen) <- newCStringLen name
  value' <- new value

  cw_len <- peek lencw
  c_snseti_
    name' value' iPrt' iSum' errors'
    cw lencw iw leniw rw lenrw
    (fromIntegral bufferlen) (8*cw_len)
  errors <- peek errors'

  free iPrt'
  free iSum'
  free errors'
  free name'
  free value'

  when (errors /= 0) $ error $ "setOption: " ++ show name ++ " = " ++ show value

mallocWorkspace :: SnInteger -> SnInteger -> SnInteger -> IO Workspace
mallocWorkspace lencw leniw lenrw = do
  ncw <- new lencw
  niw <- new leniw
  nrw <- new lenrw
  cw <- mallocArray (8*(fromIntegral lencw))
  iw <- mallocArray (fromIntegral leniw)
  rw <- mallocArray (fromIntegral lenrw)
  return $ Workspace cw iw rw ncw niw nrw

freeWorkspace :: Workspace -> IO ()
freeWorkspace (Workspace cw iw rw ncw niw nrw) = do
  mapM_ free [ncw,niw,nrw]
  free cw
  free iw
  free rw


makeSnX :: [(SnDoubleReal, (SnDoubleReal, SnDoubleReal))] -> IO SnX
makeSnX xWithBnds = do
  let nx = fromIntegral (length xWithBnds)
      (x0',xlow',xupp') = unzip3 $ map (\(x0,(xlb,xub)) -> (x0,xlb,xub)) xWithBnds
  x0'' <- newArray x0'
  xlow'' <- newArray xlow'
  xupp'' <- newArray xupp'
  nx' <- new nx
  xstate <- newArray (replicate (length xWithBnds) 0)
  xmul <- newArray (replicate (length xWithBnds) 0)

  return $ SnX { sx_nx = nx'
               , sx_xlow = xlow''
               , sx_xupp = xupp''
               , sx_x = x0''
               , sx_xstate = xstate
               , sx_xmul = xmul
               }

makeSnF :: SnInteger -> SnDoubleReal -> [(SnDoubleReal, SnDoubleReal)] -> U_fp -> IO SnF
makeSnF objrow objadd fBnds userf = do
  let nf = fromIntegral (length fBnds)
      (flb,fub) = unzip fBnds
  f' <- newArray (replicate nf 0) -- user should set this, in general
  flb' <- newArray flb
  fub' <- newArray fub
  nf' <- new (fromIntegral nf)

  fstate' <- newArray (replicate nf 0)
  fmul' <- newArray (replicate nf 0)

  userf' <- wrap userf
  objrow' <- new objrow
  objadd' <- new objadd

  return $ SnF { sf_nF = nf'
               , sf_flow = flb'
               , sf_fupp = fub'
               , sf_fstate = fstate'
               , sf_fmul = fmul'
               , sf_f = f'
               , sf_userf = userf'
               , sf_objRow = objrow'
               , sf_objAdd = objadd'
               }


snAG :: [(SnInteger, SnInteger, SnDoubleReal)] -> [(SnInteger, SnInteger)] -> IO (SnA, SnG)
snAG as gs = do
  -- even if neA,neG are zero, lenA,lenG must be > 0 so pad with 0s if neccesary
  let neA = fromIntegral $ length as
      (iA,jA,a) = unzip3 $ case as of [] -> replicate 10 (0,0,0)
                                      _ -> as
      neG = fromIntegral $ length gs
      (iG,jG) = unzip $ case gs of [] -> replicate 10 (0,0)
                                   _ -> gs

      lenA = fromIntegral (length iA) -- should lenA be > 0?? see user guide
      lenG = fromIntegral (length iG) -- should lenG be > 0?? see user guide

  putStrLn $ "neA: " ++ show neA
  putStrLn $ "neG: " ++ show neG

  putStrLn $ "lenA: " ++ show lenA
  putStrLn $ "lenG: " ++ show lenG

  neA' <- new neA
  lenA' <- new lenA
  iA' <- newArray iA
  jA' <- newArray jA
  a' <- newArray a

  lenG' <- new lenG
  neG' <- new neG
  iG' <- newArray iG
  jG' <- newArray jG

  let sna = SnA { sa_iAfun = iA', sa_jAvar = jA', sa_neA = neA', sa_lenA = lenA', sa_a = a' }
      sng = SnG { sg_iGfun = iG', sg_jGvar = jG', sg_neG = neG', sg_lenG = lenG' }
  return (sna, sng)


snInit :: Workspace -> IO ()
snInit (Workspace cw iw rw lencw leniw lenrw) = do
  iPrt' <- new global_printval
  iSum' <- new global_summaryval
  cw_len <- peek lencw
  c_sninit_ iPrt' iSum' cw lencw iw leniw rw lenrw (cw_len*8)
  free iPrt'
  free iSum'

snJac :: Workspace -> SnX -> SnF -> SnA -> SnG -> IO (SnInteger,SnInteger,SnInteger,SnInteger)
snJac workspace
  (SnX {sx_x = x, sx_xlow = xlow, sx_xupp = xupp, sx_nx = nx})
  (SnF {sf_userf = userf, sf_nF = nF})
  (SnA {sa_iAfun = iAfun, sa_jAvar = jAvar, sa_lenA = lenA, sa_neA = neA, sa_a = a})
  (SnG {sg_iGfun = iGfun, sg_jGvar = jGvar, sg_lenG = lenG, sg_neG = neG})
  = do
  let Workspace cu iu ru lencu leniu lenru = workspace
      Workspace cw iw rw lencw leniw lenrw = workspace

  info  <- new 0
  mincw <- new 0
  miniw <- new 0
  minrw <- new 0

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

  info'  <- peek info
  mincw' <- peek mincw
  miniw' <- peek miniw
  minrw' <- peek minrw

  free info
  free mincw
  free miniw
  free minrw

  when (info' /= 102) $ error $ "snJac: failure"
  return (info', mincw', miniw', minrw')

snopta :: Workspace -> SnX -> SnF -> SnA -> SnG -> IO SnInteger
snopta workspace
  (SnX {sx_x = x, sx_xlow = xlow, sx_xupp = xupp, sx_nx = nx, sx_xstate = xstate, sx_xmul = xmul})
  (SnF { sf_nF = nF, sf_fstate = fstate, sf_objAdd = objAdd, sf_objRow = objRow, sf_userf = userf
       , sf_flow = flow, sf_fupp = fupp, sf_f = f, sf_fmul = fmul })
  (SnA {sa_iAfun = iAfun, sa_jAvar = jAvar, sa_lenA = lenA, sa_neA = neA, sa_a = a})
  (SnG {sg_iGfun = iGfun, sg_jGvar = jGvar, sg_lenG = lenG, sg_neG = neG})
  = do
    putStrLn "running snopt a, woo"

    let Workspace cu iu ru lencu leniu lenru = workspace
        Workspace cw iw rw lencw leniw lenrw = workspace
    let nxname' = 1
        nfname' = 1

    nxname <- new nxname'
    nfname <- new nfname'

    xnames <- mallocArray (8*(fromIntegral nxname'))
    fnames <- mallocArray (8*(fromIntegral nfname'))
    start <- new 0
    ns <- malloc
    ninf <- malloc
    sinf <- malloc
    info <- malloc

    (probstr,prob_len) <- newCStringLen "sntoywoo"
    when (prob_len /= 8) $ error $ "problem name is not 8 chars (" ++ show prob_len ++ ")"

    cu_len' <- peek lencu
    cw_len' <- peek lencw

    mincw <- new 0
    miniw <- new 0
    minrw <- new 0

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

    info' <- peek info
    ns' <- peek ns
    ninf' <- peek ninf
    sinf' <- peek sinf
    peek mincw >>= (putStrLn . ("snopta: mincw: " ++) . show)
    peek miniw >>= (putStrLn . ("snopta: miniw: " ++) . show)
    peek minrw >>= (putStrLn . ("snopta: minrw: " ++) . show)
    putStrLn $ "info: " ++ show info'
    putStrLn $ "ns: " ++ show ns'
    putStrLn $ "ninf: " ++ show ninf'
    putStrLn $ "sinf: " ++ show sinf'

    free nxname
    free nfname
    free xnames
    free fnames
    free start
    free ns
    free ninf
    free sinf
    free info
    free probstr

    return info'


main :: IO ()
main = do
  workspace <- mallocWorkspace 500 10000 20000
  snInit workspace

  ----------- toy0 --------
  let x = [ (1, (    0, 1e12))
          , (1, (-1e12, 1e12))
          ]
      f = [ (-1e12, 1e12)
          , (-1e12, 4)
          , (-1e12, 5)
          ]
      objRow = 1
      objAdd = 0

      userf _ _ x' _ _ f' _ _ _ _ _ _ _ _ _ = do
        x0 <- peekElemOff x' 0
        x1 <- peekElemOff x' 1
        pokeElemOff f' 0 (x1)
        pokeElemOff f' 1 (x0*x0 + 4*x1*x1)
        pokeElemOff f' 2 ((x0 - 2)*(x0 - 2) + x1*x1)

  snX <- makeSnX x
  snF <- makeSnF objRow objAdd f userf

  let a = []
      g = []
  (snA,snG) <- snAG a g

  snJac workspace snX snF snA snG >>= (putStrLn . ("snJac: info: " ++) . show)

  setOptionI workspace "Derivative option" 0

  snopta workspace snX snF snA snG >>= (putStrLn . ("snopta: ret: " ++) . show)
