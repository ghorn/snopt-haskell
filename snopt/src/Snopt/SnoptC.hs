{-# OPTIONS_GHC -Wall #-}
--{-# Language TemplateHaskell #-}

module Snopt.SnoptC
       ( WhichOnes(..)
       , UserRet(..)
       , snoptc
       , sninit
       , snmemb
       , snseti
       , snsetr
       , go
       ) where

import Control.Monad ( unless )
import Data.Vector.Storable ( Vector )
import Data.Vector.Storable.Mutable ( IOVector )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import Snopt.FFI

sninit :: Int
       -> Int
       -> Int
       -> Int
       -> Int
       -> IO (IOVector CChar, IOVector SnInteger, IOVector SnDoubleReal)
sninit iprint isumm lencw leniw lenrw = do
  let lencw8 = 8*lencw

  cw <- VM.new lencw8
  iw <- VM.new leniw
  rw <- VM.new lenrw

  with (fromIntegral iprint) $ \iprint' ->
    with (fromIntegral isumm) $ \isumm' ->
    with (fromIntegral lencw) $ \lencw' ->
    with (fromIntegral leniw) $ \leniw' ->
    with (fromIntegral lenrw) $ \lenrw' ->
    VM.unsafeWith cw $ \cw' ->
    VM.unsafeWith iw $ \iw' ->
    VM.unsafeWith rw $ \rw' ->
    c_sninit_ iprint' isumm' cw' lencw' iw' leniw' rw' lenrw' (fromIntegral lencw8)

  return (cw,iw,rw)

snmemb :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IOVector SnChar -> IOVector SnInteger -> IOVector SnDoubleReal -> IO (Int,Int,Int,Int)
snmemb m n ne negCon nnCon nnJac nnObj cw iw rw = do
  let lencw8 = VM.length cw
      lencw = lencw8 `div` 8
      leniw = VM.length iw
      lenrw = VM.length rw
  with 0 $ \iexit' ->
    with (fromIntegral m)      $ \m' ->
    with (fromIntegral n)      $ \n' ->
    with (fromIntegral ne    ) $ \ne' ->
    with (fromIntegral negCon) $ \negCon' ->
    with (fromIntegral nnCon)  $ \nnCon' ->
    with (fromIntegral nnJac)  $ \nnJac' ->
    with (fromIntegral nnObj)  $ \nnObj' ->
    with 0 $ \mincw' ->
    with 0 $ \miniw' ->
    with 0 $ \minrw' ->
    with (fromIntegral lencw) $ \lencw' ->
    with (fromIntegral leniw) $ \leniw' ->
    with (fromIntegral lenrw) $ \lenrw' ->
    VM.unsafeWith cw $ \cw' ->
    VM.unsafeWith iw $ \iw' ->
    VM.unsafeWith rw $ \rw' -> do
      c_snmemb_ iexit' m' n' ne' negCon' nnCon' nnJac' nnObj' mincw' miniw' minrw' cw' lencw' iw' leniw' rw' lenrw' (fromIntegral lencw8)
      iexit <- peek iexit'
      mincw <- peek mincw'
      miniw <- peek miniw'
      minrw <- peek minrw'
      return (fromIntegral iexit, fromIntegral mincw, fromIntegral miniw, fromIntegral minrw)


snoptc :: String -- Ptr SnChar -- char *start
       -> Int -- Ptr SnInteger -- integer *m
       -> Int -- Ptr SnInteger -- integer *n
       -> Int -- Ptr SnInteger -- integer *ne
--       -> Int -- Ptr SnInteger -- integer *nName
       -> Int -- Ptr SnInteger -- integer *nnCon
       -> Int -- integer *nnObj
       -> Int -- integer *nnJac
       -> Int -- integer *iObj
       -> Double -- doublereal *ObjAdd

       -> String -- char *prob
--       -> FunPtr UsrfunC -- U_fp userfun
       -> (Vector Double -> WhichOnes -> IO (Double, Vector Double, Vector Double, Vector Double, UserRet))
       -> Vector Double -- doublereal jcol[ne]
       -> Vector Int -- integer indJ[ne]
       -> Vector Int -- integer locJ[n+1]
       -> Vector Double -- doublereal bl[n+m]
       -> Vector Double -- doublereal bu[n+m]
--       -> Ptr SnChar -- char names[nName]
       -> Vector Int -- integer hs[n+m]
       -> Vector Double -- doublereal x[n+m]
       -> Vector Double -- doublereal pi[m]

       -> IOVector SnChar -- char cw[lencw]
       -> IOVector SnInteger -- integer iw[leniw]
       -> IOVector SnDoubleReal -- doublereal rw[lenrw]
       -> IO ((Vector Double, Vector Double, Vector Double, Vector CInt),
              (CInt,CInt,CInt,CInt,CInt,CInt,Double,Double))
snoptc start m n ne nnCon nnObj nnJac iObj objAdd prob hsUsrfun
  jCol indJ locJ bl bu hs0 x0 pi0
  cw iw rw = do
    unless (m > 0) $ error $ "snoptc: m must be > 0, your m == " ++ show m
    unless (n > 0) $ error $ "snoptc: n must be > 0, your n == " ++ show n
    unless (ne > 0) $ error $ "snoptc: ne must be > 0, your ne == " ++ show ne

    unless (V.length indJ == ne)    $ error $ "snoptc: (V.length indJ == ne):   " ++ show (V.length indJ, ne)
    unless (V.length hs0 == n+m)    $ error $ "snoptc: (V.length hs0 == n+m):   " ++ show (V.length hs0, n+m)
    unless (V.length locJ == n+1)   $ error $ "snoptc: (V.length locJ == n+1):  " ++ show (V.length locJ, n+1)
    unless (V.length jCol == ne)    $ error $ "snoptc: (V.length jCol == ne):   " ++ show (V.length jCol, ne)
    unless (V.length bl == n + m)   $ error $ "snoptc: (V.length bl == n + m):  " ++ show (V.length bl, n + m)
    unless (V.length bu == n + m)   $ error $ "snoptc: (V.length bu == n + m):  " ++ show (V.length bu, n + m)
    unless (V.length pi0 == m)      $ error $ "snoptc: (V.length pi0 == m):     " ++ show (V.length pi0, m)
    unless (V.length x0 == (n + m)) $ error $ "snoptc: (V.length x0 == (n + m)):" ++ show (V.length x0, n + m)

    unless (V.head locJ == 1) $ error "snoptc: locJ[0] /= 1"

    let lencw8 = VM.length cw
        lencw = lencw8 `div` 8
        leniw = VM.length iw
        lenrw = VM.length rw


        nName = 1 :: Int
        startLen = length start
        probLen = 8

    info <- new 0
    mincw <- new 0
    miniw <- new 0
    minrw <- new 0
    nS <- new 0
    nInf <- new 0
    sInf <- new 0
    obj <- new 0

    pii <- V.thaw (V.map realToFrac pi0)
    hs <- V.thaw (V.map fromIntegral hs0)
    x <- V.thaw (V.map realToFrac x0)
    rc <- VM.new (n + m)

    usrfun <- wrapC (usrfunWrapper hsUsrfun)
    withCString start $ \start' ->
      with (fromIntegral m) $ \m' ->
      with (fromIntegral n) $ \n' ->
      with (fromIntegral ne) $ \ne' ->
      with (fromIntegral nName) $ \nName' ->
      with (fromIntegral nnCon) $ \nnCon' ->
      with (fromIntegral nnObj) $ \nnObj' ->
      with (fromIntegral nnJac) $ \nnJac' ->
      with (fromIntegral iObj) $ \iObj' ->

      with (realToFrac objAdd) $ \objAdd' ->

      withCString (take 8 (prob ++ (repeat ' '))) $ \prob' ->

      V.unsafeWith (V.map realToFrac jCol) $ \jCol' ->
      V.unsafeWith (V.map fromIntegral indJ) $ \indJ' ->
      V.unsafeWith (V.map fromIntegral locJ) $ \locJ' ->
      V.unsafeWith (V.map realToFrac bl) $ \bl' ->
      V.unsafeWith (V.map realToFrac bu) $ \bu' ->
      V.unsafeWith (V.empty) $ \names' ->
      VM.unsafeWith hs $ \hs' ->
      VM.unsafeWith x $ \x' ->
      VM.unsafeWith pii $ \pi' ->
      VM.unsafeWith rc $ \rc' ->

      with (fromIntegral lencw) $ \lencw' ->
      with (fromIntegral leniw) $ \leniw' ->
      with (fromIntegral lenrw) $ \lenrw' ->
      VM.unsafeWith cw $ \cw' ->
      VM.unsafeWith iw $ \iw' ->
      VM.unsafeWith rw $ \rw' ->
        c_snoptc_ start' m' n' ne' nName' nnCon' nnObj' nnJac' iObj' objAdd'
        prob' usrfun
        jCol' indJ' locJ' bl' bu' names' hs' x' pi' rc'
        info
        mincw miniw minrw
        nS nInf sInf obj
        cw' lencw' iw' leniw' rw' lenrw'
        cw' lencw' iw' leniw' rw' lenrw'
        (fromIntegral startLen) probLen 0 (fromIntegral lencw8) (fromIntegral lencw8)

    freeHaskellFunPtr usrfun

    let peekFree peekMe = do
          ret <- peek peekMe
          free peekMe
          return ret

    info' <- peekFree info
    mincw' <- peekFree mincw
    miniw' <- peekFree miniw
    minrw' <- peekFree minrw
    nS' <- peekFree nS
    nInf' <- peekFree nInf
    sInf' <- peekFree sInf
    obj' <- peekFree obj

    piF <- V.unsafeFreeze pii :: IO (Vector Double)
    rcF <- V.unsafeFreeze rc :: IO (Vector Double)
    hsF <- V.unsafeFreeze hs :: IO (Vector CInt)
    xF <- V.unsafeFreeze x :: IO (Vector Double)
    return ((xF,piF,rcF,hsF),(info',mincw',miniw',minrw',nS',nInf',sInf',obj'))


snset :: Storable b => (a -> b)
         -> (Ptr CChar -> Ptr b -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr CInt -> SnFtnLen -> SnFtnLen -> IO ())
         -> IOVector CChar -> IOVector CInt -> IOVector Double
         -> Int -> Int -> String -> a -> IO Int
snset convert c_set cw iw rw iprint isumm name value = do
  let lencw8 = fromIntegral (VM.length cw)
      lencw = lencw8 `div` 8
      leniw = fromIntegral (VM.length iw)
      lenrw = fromIntegral (VM.length rw)

  errors' <- new 0
  (name',bufferlen) <- newCStringLen name
  value' <- new (convert value)

  with lencw $ \lencw' ->
    with leniw $ \leniw' ->
    with lenrw $ \lenrw' ->
    with (fromIntegral iprint) $ \iprint' ->
    with (fromIntegral isumm) $ \isumm' ->
    VM.unsafeWith cw $ \cw' ->
    VM.unsafeWith iw $ \iw' ->
    VM.unsafeWith rw $ \rw' ->
    c_set name' value' iprint' isumm' errors'
    cw' lencw' iw' leniw' rw' lenrw'
    (fromIntegral bufferlen) lencw8
  errors <- peek errors'

  free errors'
  free name'
  free value'

  return (fromIntegral errors)

snseti :: IOVector CChar
          -> IOVector CInt
          -> IOVector Double
          -> Int
          -> Int
          -> String
          -> Int
          -> IO Int
snseti = snset fromIntegral c_snseti_

snsetr :: IOVector CChar
          -> IOVector CInt
          -> IOVector Double
          -> Int
          -> Int
          -> String
          -> Double
          -> IO Int
snsetr = snset realToFrac c_snsetr_


usrfunWrapper
  :: (V.Vector Double -> WhichOnes -> IO (Double, V.Vector Double, V.Vector Double, V.Vector Double, UserRet))
    -> Ptr SnInteger   --  ( integer *mode
     -> Ptr SnInteger    --  , integer *nnObj
     -> Ptr SnInteger    --  , integer *nnCon
     -> Ptr SnInteger    --  , integer *nnJac
     -> Ptr SnInteger    --  , integer *nnL
     -> Ptr SnInteger    --  , integer *neJac

     -> Ptr SnDoubleReal --  , doublereal x[nnL]
     -> Ptr SnDoubleReal --  , doublereal *fObj
     -> Ptr SnDoubleReal --  , doublereal gObj[nnObj]
     -> Ptr SnDoubleReal --  , doublereal fCon[nnCon]
     -> Ptr SnDoubleReal --  , doublereal gCon[neJac]

     -> Ptr SnInteger    --  , integer *nState

     -> Ptr SnChar       --  , char       *cu
     -> Ptr SnInteger    --  , integer *lencu
     -> Ptr SnInteger    --  , integer    iu[]
     -> Ptr SnInteger    --  , integer *leniu
     -> SnDoubleReal     --  , doublereal ru[]
     -> SnInteger        --  , integer *lenru
     -> IO ()
usrfunWrapper
  userfun
  mode' nnObj' nnCon' _nnJac' nnL' neJac'
  x' fObj' gObj' fCon' gCon'
  _nState' _ _ _ _ _ _ = do
    modeInt <- peek mode'
    let mode = case modeInt of
          0 -> FunOnly
          1 -> DerivOnly
          2 -> Both
          k -> error $ "snoptc callback got unknown mode: " ++ show k

    nnObj <- fmap fromIntegral (peek nnObj')
    nnCon <- fmap fromIntegral (peek nnCon')
    --nnJac <- fmap fromIntegral (peek nnJac')
    neJac <- fmap fromIntegral (peek neJac')
    nnL <- fmap fromIntegral (peek nnL')

    x <- fmap (flip V.unsafeFromForeignPtr0 nnL) (newForeignPtr_ x')

    gObjVec <- fmap (flip VM.unsafeFromForeignPtr0 nnObj) (newForeignPtr_ gObj')
    fConVec <- fmap (flip VM.unsafeFromForeignPtr0 nnCon) (newForeignPtr_ fCon')
    gConVec <- fmap (flip VM.unsafeFromForeignPtr0 neJac) (newForeignPtr_ gCon')

    (fObj,gObj,fCon,gCon,keepGoing) <- userfun x mode

    poke fObj' fObj
    if (V.length gObj == VM.length gObjVec)
      then V.copy gObjVec gObj
      else error $ "gObj dim error, " ++ show (V.length gObj, VM.length gObjVec)
    if (V.length fCon == VM.length fConVec)
      then V.copy fConVec fCon
      else error $ "fCon dim error, " ++ show (V.length fCon, VM.length fConVec)
    if (V.length gCon == VM.length gConVec)
      then V.copy gConVec gCon
      else error $ "gCon dim error, " ++ show (V.length gCon, VM.length gConVec)

    case keepGoing of
         KeepGoing -> return ()
         ReduceAlpha -> poke mode' (-1)
         StopIterating -> poke mode' (-2)

    return ()

data WhichOnes = FunOnly | DerivOnly | Both
data UserRet = KeepGoing | ReduceAlpha | StopIterating

---------------------------------------------------------------------------------
------------------------------------ sntoyc -------------------------------------
---------------------------------------------------------------------------------
usrfunC
  :: Vector Double -> WhichOnes -> (Double, Vector Double, Vector Double, Vector Double, UserRet)
usrfunC x _ = (fObj, gObj, fCon, gCon, KeepGoing)
  where
    x1 = x V.! 0
    x2 = x V.! 1
    x3 = x V.! 2

    sum' = x1 + x2 + x3
    fObj = sum'*sum'

    gObj = V.fromList [ 2*sum'
                      , 2*sum'
                      , 2*sum'
                      ]
    fCon = V.fromList [ x1**2 + x2**2
                      , x2**4
                      ]
    gCon = V.fromList [ 2*x1
                      , 2*x2
                      , 4*x2**3
                      ]

go :: IO ()
go = do
  let iPrint = 0 -- 9
      iSumm = 6
      m = 4
      n = 4
      ne = 9
      nnCon = 2
      nnJac = 2
      nnObj = 3
      iObj = 4
      objAdd = 0

      locA = V.fromList [1,3,6,8,ne+1]
      aCol = V.fromList [0,2,0,0,4,1,3,1,5]
      indA = V.fromList [1,3,1,2,3,1,iObj,2,iObj]
      inf = 1e20
      bl = V.fromList [-inf, -inf,   0,   0, 2, 4,   0, -inf]
      bu = V.fromList [ inf,  inf, inf, inf, 2, 4, inf, inf]
      x = V.fromList [0.1,0.125,0.666666,0.142857,0,0,0,0]
      pii = V.fromList $ replicate m 0.0
      hs = V.fromList $ replicate (n+m) 0

  let usrfun xvec = return . (usrfunC xvec)

  (cw,iw,rw) <- sninit iPrint iSumm 500 10000 20000
  _ <- snseti cw iw rw iPrint iSumm "Iterations" 250

  ((xF,_,_,_),_) <- snoptc "cold" m n ne nnCon nnObj nnJac iObj objAdd "hstoyc" usrfun aCol indA locA bl bu hs x pii cw iw rw
  print xF

  return ()
