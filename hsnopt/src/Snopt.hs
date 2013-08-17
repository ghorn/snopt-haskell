{-# OPTIONS_GHC -Wall #-}

--module Snopt ( Workspace(..)
--             , mallocWorkspace
--             , freeWorkspace
--             ) where

import Control.Monad ( when )
--import Foreign.C ( CInt(..), CDouble(..), CChar(..) )
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.Marshal
import Foreign.Storable

import Snopt.Bindings

--foreign import ccall "wrapper"
--  wrap :: U_fp -> IO (FunPtr U_fp)

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
