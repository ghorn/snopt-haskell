{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main ( main, mallocWorkspace, freeWorkspace, Workspace(..) ) where

import Control.Monad ( when )
import Foreign.C ( CInt(..), CDouble(..), CChar(..) )
import Foreign.C.String
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Marshal
import Foreign.Storable

type SnInteger = CInt
type SnDoubleReal = CDouble
type SnChar = CChar
type SnFtnLen = CInt
--type SnInt = CInt

type Snopta_ = Ptr SnInteger -- ( integer *start
               -> Ptr SnInteger    -- , integer *nef
               -> Ptr SnInteger    -- , integer *n
               -> Ptr SnInteger    -- , integer *nxname
               -> Ptr SnInteger    -- , integer *nfname
               -> Ptr SnDoubleReal    -- , doublereal *obja
               -> Ptr SnInteger    -- , integer *objrow
               -> Ptr SnChar    -- , char *prob
               -> FunPtr U_fp    -- , U_fp usrfun
               -> Ptr SnInteger    -- , integer *iafun
               -> Ptr SnInteger    -- , integer *javar
               -> Ptr SnInteger    -- , integer *lena
               -> Ptr SnInteger    -- , integer *nea
               -> Ptr SnDoubleReal    -- , doublereal *a
               -> Ptr SnInteger    -- , integer *igfun
               -> Ptr SnInteger    -- , integer *jgvar
               -> Ptr SnInteger    -- , integer *leng
               -> Ptr SnInteger    -- , integer *neg
               -> Ptr SnDoubleReal    -- , doublereal *xlow
               -> Ptr SnDoubleReal    -- , doublereal *xupp
               -> Ptr SnChar    -- , char *xnames
               -> Ptr SnDoubleReal    -- , doublereal *flow
               -> Ptr SnDoubleReal    -- , doublereal *fupp
               -> Ptr SnChar    -- , char *fnames
               -> Ptr SnDoubleReal    -- , doublereal *x
               -> Ptr SnInteger    -- , integer *xstate
               -> Ptr SnDoubleReal    -- , doublereal *xmul
               -> Ptr SnDoubleReal    -- , doublereal *f
               -> Ptr SnInteger    -- , integer *fstate
               -> Ptr SnDoubleReal    -- , doublereal *fmul
               -> Ptr SnInteger    -- , integer *inform
               -> Ptr SnInteger    -- , integer *mincw
               -> Ptr SnInteger    -- , integer *miniw
               -> Ptr SnInteger    -- , integer *minrw
               -> Ptr SnInteger    -- , integer *ns
               -> Ptr SnInteger    -- , integer *ninf
               -> Ptr SnDoubleReal    -- , doublereal *sinf
               -> Ptr SnChar    -- , char *cu
               -> Ptr SnInteger    -- , integer *lencu
               -> Ptr SnInteger    -- , integer *iu
               -> Ptr SnInteger    -- , integer *leniu
               -> Ptr SnDoubleReal    -- , doublereal *ru
               -> Ptr SnInteger    -- , integer *lenru
               -> Ptr SnChar    -- , char *cw
               -> Ptr SnInteger    -- , integer *lencw
               -> Ptr SnInteger    -- , integer *iw
               -> Ptr SnInteger    -- , integer *leniw
               -> Ptr SnDoubleReal    -- , doublereal *rw
               -> Ptr SnInteger    -- , integer *lenrw
               -> SnFtnLen    -- , ftnlen prob_len
               -> SnFtnLen    -- , ftnlen xnames_le
               -> SnFtnLen    -- , ftnlen fnames_le
               -> SnFtnLen    -- , ftnlen cu_len
               -> SnFtnLen    -- , ftnlen cw_len );
               -> IO () -- extern int snopta_
foreign import ccall safe "snopta_" c_snopta_ :: Snopta_

type SnInit_ = Ptr SnInteger -- ( integer *iPrint
               -> Ptr SnInteger -- , integer *iSumm
               -> Ptr SnChar -- , char *cw
               -> Ptr SnInteger -- , integer *lencw
               -> Ptr SnInteger -- , integer *iw
               -> Ptr SnInteger -- , integer *leniw
               -> Ptr SnDoubleReal -- , doublereal *rw
               -> Ptr SnInteger -- , integer *lenrw
               -> SnFtnLen -- , ftnlen cw_len)
               -> IO () -- SnInt -- extern int sninit_
foreign import ccall unsafe "sninit_" c_sninit_ :: SnInit_

type SnSet_ a = Ptr SnChar -- ( char *buffer
                -> Ptr a         -- , integer *{ri}value
                -> Ptr SnInteger -- , integer *iprint
                -> Ptr SnInteger -- , integer *isumm
                -> Ptr SnInteger -- , integer *inform
                -> Ptr SnChar -- , char *cw
                -> Ptr SnInteger -- , integer *lencw
                -> Ptr SnInteger -- , integer *iw
                -> Ptr SnInteger -- , integer *leniw
                -> Ptr SnDoubleReal -- , doublereal *rw
                -> Ptr SnInteger -- , integer *lenrw
                -> SnFtnLen -- , ftnlen buffer_len
                -> SnFtnLen -- , ftnlen cw_len );
                -> IO () -- extern int snseti_

--type SnSeti_ = SnSet_ SnInteger
--type SnSetr_ = SnSet_ SnDoubleReal
foreign import ccall unsafe "snseti_" c_snseti_ :: SnSet_ SnInteger
foreign import ccall unsafe "snsetr_" c_snsetr_ :: SnSet_ SnDoubleReal

type SnJac_ = Ptr SnInteger -- ( integer *iExit
              -> Ptr SnInteger -- , integer *nef
              -> Ptr SnInteger -- , integer *n
              -> FunPtr U_fp -- , U_fp userfg
              -> Ptr SnInteger -- , integer *iafun
              -> Ptr SnInteger -- , integer *javar
              -> Ptr SnInteger -- , integer *lena
              -> Ptr SnInteger -- , integer *nea
              -> Ptr SnDoubleReal -- , doublereal *a
              -> Ptr SnInteger -- , integer *igfun
              -> Ptr SnInteger -- , integer *jgvar
              -> Ptr SnInteger -- , integer *leng
              -> Ptr SnInteger -- , integer *neg
              -> Ptr SnDoubleReal -- , doublereal *x
              -> Ptr SnDoubleReal -- , doublereal *xlow
              -> Ptr SnDoubleReal -- , doublereal *xupp
              -> Ptr SnInteger -- , integer *mincw
              -> Ptr SnInteger -- , integer *miniw
              -> Ptr SnInteger -- , integer *minrw
              -> Ptr SnChar -- , char *cu
              -> Ptr SnInteger -- , integer *lencu
              -> Ptr SnInteger -- , integer *iu
              -> Ptr SnInteger -- , integer *leniu
              -> Ptr SnDoubleReal -- , doublereal *ru
              -> Ptr SnInteger -- , integer *lenru
              -> Ptr SnChar -- , char *cw
              -> Ptr SnInteger -- , integer *lencw
              -> Ptr SnInteger -- , integer *iw
              -> Ptr SnInteger -- , integer *leniw
              -> Ptr SnDoubleReal -- , doublereal *rw
              -> Ptr SnInteger -- , integer *lenrw
              -> SnFtnLen -- , ftnlen cu_len
              -> SnFtnLen -- , ftnlen cw_len);
              -> IO () -- extern int snjac_
foreign import ccall safe "snjac_" c_snjac_ :: SnJac_

type U_fp = Ptr SnInteger       --  ( integer    *Status
            -> Ptr SnInteger    --  , integer *n
            -> Ptr SnDoubleReal --  , doublereal x[]
            -> Ptr SnInteger    --  , integer    *needF
            -> Ptr SnInteger    --  , integer *neF
            -> Ptr SnDoubleReal --  , doublereal F[]
            -> Ptr SnInteger    --  , integer    *needG
            -> Ptr SnInteger    --  , integer *neG
            -> Ptr SnDoubleReal --  , doublereal G[]
            -> Ptr SnChar       --  , char       *cu
            -> Ptr SnInteger    --  , integer *lencu
            -> Ptr SnInteger    --  , integer    iu[]
            -> Ptr SnInteger    --  , integer *leniu
            -> SnDoubleReal     --  , doublereal ru[]
            -> SnInteger        --  , integer *lenru )
            -> IO () -- int toyusrf_

foreign import ccall "wrapper"
  wrap :: U_fp -> IO (FunPtr U_fp)

data Workspace = Workspace { wsCw :: Ptr SnChar
                           , wsIw :: Ptr SnInteger
                           , wsRw :: Ptr SnDoubleReal
                           , wsNcw :: Ptr SnInteger
                           , wsNiw :: Ptr SnInteger
                           , wsNrw :: Ptr SnInteger
                           }
mallocWorkspace :: SnInteger -> SnInteger -> SnInteger -> IO Workspace
mallocWorkspace lencw leniw lenrw = do
  ncw <- new lencw
  niw <- new leniw
  nrw <- new lenrw
  cw <- mallocArray (fromIntegral lencw)
  iw <- mallocArray (fromIntegral leniw)
  rw <- mallocArray (fromIntegral lenrw)
  return $ Workspace cw iw rw ncw niw nrw

freeWorkspace :: Workspace -> IO ()
freeWorkspace (Workspace cw iw rw ncw niw nrw) = do
  mapM_ free [ncw,niw,nrw]
  free cw
  free iw
  free rw

printval :: SnInteger
printval = 0

summaryval :: SnInteger
summaryval = 6


userf' :: U_fp
userf' _ _ x _ _ f _ _ _ _ _ _ _ _ _ = do
  x0 <- peekElemOff x 0
  x1 <- peekElemOff x 1
  pokeElemOff f 0 (x1)
  pokeElemOff f 1 (x0*x0 + 4*x1*x1)
  pokeElemOff f 2 ((x0 - 2)*(x0 - 2) + x1*x1)

setOptionI :: Workspace -> String -> SnInteger -> IO ()
setOptionI (Workspace cw iw rw lencw leniw lenrw) name value = do
  iPrt' <- new printval
  iSum' <- new summaryval
  errors' <- new 0
  (name',bufferlen) <- newCStringLen name
  value' <- new value

  cw_len <- peek lencw
  c_snseti_ name' value' iPrt' iSum' errors' cw lencw iw leniw rw lenrw (fromIntegral bufferlen) cw_len
  errors <- peek errors'

  free iPrt'
  free iSum'
  free errors'
  free name'
  free value'

  when (errors /= 0) $ error $ "setOption: " ++ show name ++ " = " ++ show value

snInit :: Workspace -> IO ()
snInit (Workspace cw iw rw lencw leniw lenrw) = do
  iPrt' <- new printval
  iSum' <- new summaryval
  cw_len <- peek lencw
  c_sninit_ iPrt' iSum' cw lencw iw leniw rw lenrw cw_len
  free iPrt'
  free iSum'

main :: IO ()
main = do
  workspace <- mallocWorkspace (8*500) 10000 20000
  snInit workspace

  ----------- toy0 --------
  nF <- new 3
  objRow <- new (1::SnInteger)
  n <- new 2
  objAdd <- new (0::SnDoubleReal)
  xlow <- newArray [    0, -1e12]
  xupp <- newArray [ 1e12,  1e12]
  fstate <- newArray [0,0,0]
  flow <- newArray [-1e12,-1e12,-1e12::SnDoubleReal]
  fupp <- newArray [ 1e12, 4, 5::SnDoubleReal]
  fmul <- newArray [0,0,0::SnDoubleReal]
  f <- newArray [0,0,0::SnDoubleReal]
  x <- newArray [1,1]
  (prob,prob_len) <- newCStringLen "sntoy woo"

  userf <- wrap userf'

  let Workspace cu iu ru lencu leniu lenru = workspace
      Workspace cw iw rw lencw leniw lenrw = workspace
      lenA' = 10
      lenG' = 10
  mincw <- malloc
  miniw <- malloc
  minrw <- malloc
  info <- malloc
  lenA <- new lenA'
  lenG <- new lenG'
  neG <- malloc
  neA <- malloc
  a <- mallocArray (fromIntegral lenA')
  iAfun <- mallocArray (fromIntegral lenA')
  jAvar <- mallocArray (fromIntegral lenA')
  jGvar <- mallocArray (fromIntegral lenG')
  iGfun <- mallocArray (fromIntegral lenG')
  cu_len <- peek lencu
  cw_len <- peek lencw
  c_snjac_ info nF n userf
    iAfun jAvar lenA neA a
    iGfun jGvar lenG neG
    x xlow xupp mincw miniw minrw
    cu lencu iu leniu ru lenru
    cw lencw iw leniw rw lenrw
    cu_len cw_len
  info' <- peek info
  putStrLn $ "info: " ++ show info'

  setOptionI workspace "Derivative option" 0


  ----------------------- SNOPTA -------------------
  let nxname' = 1
      nfname' = 1
      maxn = 10
  xstate <- newArray [0,0]
  xmul <- mallocArray maxn
  nxname <- new nxname'
  xnames <- mallocArray (fromIntegral nxname')
  nfname <- new nfname'
  fnames <- mallocArray (fromIntegral nfname')
  start <- new 0
  ns <- malloc
  ninf <- malloc
  sinf <- malloc
  cu_len' <- peek lencu
  cw_len' <- peek lencw
  c_snopta_ start nF n nxname nfname
    objAdd objRow prob userf
    iAfun jAvar lenA neA a
    iGfun jGvar lenG neG
    xlow xupp xnames flow fupp fnames
    x xstate xmul f fstate fmul
    info mincw miniw minrw
    ns ninf sinf
    cu lencu iu leniu ru lenru
    cw lencw iw leniw rw lenrw
    (fromIntegral prob_len) (8*nxname') (8*nfname') cu_len' cw_len'
  info'' <- peek info
  ns' <- peek ns
  ninf' <- peek ninf
  sinf' <- peek sinf
  putStrLn $ "info: " ++ show info''
  putStrLn $ "ns: " ++ show ns'
  putStrLn $ "ninf: " ++ show ninf'
  putStrLn $ "sinf: " ++ show sinf'
