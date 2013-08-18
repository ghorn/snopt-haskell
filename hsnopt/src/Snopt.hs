{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}

module Snopt ( -- * types
               SnoptA
             , SnDoubleReal
             , SnInteger
               -- * memory
             , mallocSnoptA
             , freeSnoptA
               -- * the functions
             , snInit
             , setOptionI
             , snJac
             , snopta
               -- * extra
             , wrap
--             , toy0
             , toy1
             ) where

import Control.Lens ( makeLenses, (^.), over, set )
import Control.Lens.Getter
import Control.Monad ( unless, when )
import Foreign.C.String
import Foreign.Ptr ( FunPtr, Ptr )
import Foreign.Marshal
import Foreign.Storable

import Snopt.Bindings

global_printval :: SnInteger
global_printval = 0

global_summaryval :: SnInteger
global_summaryval = 6

data Workspace = Workspace
                 (Ptr SnChar) (Ptr SnInteger) (Ptr SnDoubleReal)
                 (Ptr SnInteger) (Ptr SnInteger) (Ptr SnInteger)


data SnX = SnX { _sx_nx :: Ptr SnInteger
               , _sx_xlow :: Ptr SnDoubleReal
               , _sx_xupp :: Ptr SnDoubleReal
               , _sx_x :: Ptr SnDoubleReal
               , _sx_xstate :: Ptr SnInteger
               , _sx_xmul :: Ptr SnDoubleReal
               }
data SnF = SnF { _sf_nF :: Ptr SnInteger
               , _sf_flow :: Ptr SnDoubleReal
               , _sf_fupp :: Ptr SnDoubleReal
               , _sf_fstate :: Ptr SnInteger
               , _sf_fmul :: Ptr SnDoubleReal
               , _sf_f :: Ptr SnDoubleReal
               , _sf_objRow :: Ptr SnInteger
               , _sf_objAdd :: Ptr SnDoubleReal
               }
data SnA = SnA { _sa_iAfun :: Ptr SnInteger
               , _sa_jAvar :: Ptr SnInteger
               , _sa_a :: Ptr SnDoubleReal
               , _sa_lenA :: Ptr SnInteger -- size of vector
               , _sa_neA :: Ptr SnInteger -- number of elements in vector
               }
data SnG = SnG { _sg_iGfun :: Ptr SnInteger
               , _sg_jGvar :: Ptr SnInteger
               , _sg_lenG :: Ptr SnInteger -- size of vector
               , _sg_neG :: Ptr SnInteger -- number of elements in vector
               }
data SnoptA = SnoptA { _sna_ws :: Workspace
                     , _sna_x :: SnX
                     , _sna_f :: SnF
                     , _sna_a :: SnA
                     , _sna_g :: SnG
                     , _sna_ufp :: FunPtr U_fp
                     }
makeLenses ''SnX
makeLenses ''SnF
makeLenses ''SnA
makeLenses ''SnG
makeLenses ''SnoptA

---------------------------------- malloc/free ------------------------------
mallocSnoptA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> FunPtr U_fp -> IO SnoptA
mallocSnoptA nc ni nr nx nf na ng ufp = do
  workspace <- mallocWorkspace nc ni nr
  sx <- mallocX nx
  sf <- mallocF nf
  (sa,sg) <- mallocAG na ng
  return $ SnoptA workspace sx sf sa sg ufp
  
freeSnoptA :: SnoptA -> IO ()
freeSnoptA (SnoptA ws x f a g _) = do
  freeWorkspace ws
  freeF f
  freeAG a g
  
mallocWorkspace :: Int -> Int -> Int -> IO Workspace
mallocWorkspace lencw leniw lenrw = do
  ncw <- new (fromIntegral lencw)
  niw <- new (fromIntegral leniw)
  nrw <- new (fromIntegral lenrw)
  cw <- mallocArray (8*lencw)
  iw <- mallocArray leniw
  rw <- mallocArray lenrw
  return $ Workspace cw iw rw ncw niw nrw

freeWorkspace :: Workspace -> IO ()
freeWorkspace (Workspace cw iw rw ncw niw nrw) = do
  mapM_ free [ncw,niw,nrw]
  free cw
  free iw
  free rw

mallocX :: Int -> IO SnX
mallocX n = do
  nx     <- new (fromIntegral n)
  x      <- newArray (replicate n 0)
  xlow   <- newArray (replicate n 0)
  xupp   <- newArray (replicate n 0)
  xstate <- newArray (replicate n 0)
  xmul   <- newArray (replicate n 0)
  return $ SnX { _sx_nx = nx
               , _sx_xlow = xlow
               , _sx_xupp = xupp
               , _sx_x = x
               , _sx_xstate = xstate
               , _sx_xmul = xmul
               }

freeX :: SnX -> IO ()
freeX (SnX x0 x1 x2 x3 x4 x5) = do
  free x0
  free x1
  free x2
  free x3
  free x4
  free x5

mallocF :: Int -> IO SnF
mallocF n = do
  nf  <- new (fromIntegral n)
  f      <- newArray (replicate n 0)
  flow   <- newArray (replicate n 0)
  fupp   <- newArray (replicate n 0)
  fstate <- newArray (replicate n 0)
  fmul   <- newArray (replicate n 0)
  objrow <- new 1
  objadd <- new 0

  return $ SnF { _sf_nF = nf
               , _sf_flow = flow
               , _sf_fupp = fupp
               , _sf_fstate = fstate
               , _sf_fmul = fmul
               , _sf_f = f
               , _sf_objRow = objrow
               , _sf_objAdd = objadd
               }

freeF :: SnF -> IO ()
freeF (SnF f0 f1 f2 f3 f4 f5 f6 f7) = do
  free f0
  free f1
  free f2
  free f3
  free f4
  free f5
  free f6
  free f7

mallocAG :: Int -> Int -> IO (SnA, SnG)
mallocAG neA neG
  | neA < 0 = error "mallocAG: neA < 0"
  | neG < 0 = error "mallocAG: neG < 0"
mallocAG neA neG = do
  let lenA | neA < 1 = 1
           | otherwise = neA
      lenG | neG < 1 = 1
           | otherwise = neG

  neA'  <- new (fromIntegral neA)
  lenA' <- new (fromIntegral lenA)
  iA'   <- newArray (replicate lenA 0)
  jA'   <- newArray (replicate lenA 0)
  a'    <- newArray (replicate lenA 0)

  neG'  <- new (fromIntegral neG)
  lenG' <- new (fromIntegral lenG)
  iG' <- newArray (replicate lenA 0)
  jG' <- newArray (replicate lenA 0)

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

freeAG :: SnA -> SnG -> IO ()
freeAG (SnA a0 a1 a2 a3 a4) (SnG g0 g1 g2 g3) = do
  free a0
  free a1
  free a2
  free a3
  free a4

  free g0
  free g1
  free g2
  free g3

--------------------------- setters/getters ----------------------------
getArray :: Storable a =>
            Getting (Ptr SnInteger) xfag (Ptr SnInteger)
         -> ((xfag -> Accessor xfag xfag) -> SnoptA -> Accessor xfag SnoptA)
         -> Getting (Ptr a) xfag (Ptr a)
         -> SnoptA
         -> IO [a]
getArray getNum getXFAG getField snoptA = do
  let snx  = snoptA ^. getXFAG
  n <- peek (snx ^. getNum)
  peekArray (fromIntegral n) (snx ^. getField)

setArray :: Storable a =>
            String
         -> Getting (Ptr SnInteger) xfag (Ptr SnInteger)
         -> ((xfag -> Accessor xfag xfag) -> SnoptA -> Accessor xfag SnoptA)
         -> Getting (Ptr a) xfag (Ptr a)
         -> SnoptA
         -> [a]
         -> IO ()
setArray name getNum getXFAG getField snoptA val = do
  let snx  = snoptA ^. getXFAG
  n <- peek (snx ^. getNum)
  let lval = length val
      trueval = fromIntegral n
  when (lval /= trueval) $
    error $ "setArray dimension mismatch: " ++ name ++ ": " ++ show lval ++ " /= " ++ show trueval
  pokeArray (snx ^. getField) val

getScalar :: Storable a =>
             ((xfag -> Accessor xfag xfag) -> SnoptA -> Accessor xfag SnoptA)
          -> Getting (Ptr a) xfag (Ptr a)
          -> SnoptA
          -> IO a
getScalar getXFAG getField snoptA = do
  let snx  = snoptA ^. getXFAG
  peek (snx ^. getField)

setScalar :: Storable a =>
             ((xfag -> Accessor xfag xfag) -> SnoptA -> Accessor xfag SnoptA)
          -> Getting (Ptr a) xfag (Ptr a)
          -> SnoptA
          -> a
          -> IO ()
setScalar getXFAG getField snoptA val = do
  let snx  = snoptA ^. getXFAG
  poke (snx ^. getField) val

getXlow   = getArray sx_nx sna_x sx_xlow
getXupp   = getArray sx_nx sna_x sx_xupp
getX      = getArray sx_nx sna_x sx_x
getXstate = getArray sx_nx sna_x sx_xstate
getXmul   = getArray sx_nx sna_x sx_xmul

setXlow   = setArray "xlow"   sx_nx sna_x sx_xlow
setXupp   = setArray "xupp"   sx_nx sna_x sx_xupp
setX      = setArray "x"      sx_nx sna_x sx_x
setXstate = setArray "xstate" sx_nx sna_x sx_xstate
setXmul   = setArray "xmul  " sx_nx sna_x sx_xmul

getFlow   = getArray sf_nF sna_f sf_flow
getFupp   = getArray sf_nF sna_f sf_fupp
getFstate = getArray sf_nF sna_f sf_fstate
getFmul   = getArray sf_nF sna_f sf_fmul
getF      = getArray sf_nF sna_f sf_f
getObjRow = getScalar sna_f sf_objRow
getObjAdd = getScalar sna_f sf_objAdd

setFlow   = setArray "flow"   sf_nF sna_f sf_flow
setFupp   = setArray "fupp"   sf_nF sna_f sf_fupp
setFstate = setArray "fstate" sf_nF sna_f sf_fstate
setFmul   = setArray "fmul"   sf_nF sna_f sf_fmul
setF      = setArray "f"      sf_nF sna_f sf_f
setObjRow = setScalar sna_f sf_objRow
setObjAdd = setScalar sna_f sf_objAdd

getIAfun = getArray sa_neA sna_a sa_iAfun
getJAvar = getArray sa_neA sna_a sa_jAvar
getA     = getArray sa_neA sna_a sa_a

setIAfun = setArray "iAfun" sa_neA sna_a sa_iAfun
setJAvar = setArray "jAvar" sa_neA sna_a sa_jAvar
setA     = setArray "A"     sa_neA sna_a sa_a

getIGfun = getArray sg_neG sna_g sg_iGfun
getJGvar = getArray sg_neG sna_g sg_jGvar

setIGfun = setArray "iGfun" sg_neG sna_g sg_iGfun
setJGvar = setArray "jGvar" sg_neG sna_g sg_jGvar

--------------------------------------------------------------------------------------


snInit' :: Workspace -> IO ()
snInit' (Workspace cw iw rw lencw leniw lenrw) = do
  iPrt' <- new global_printval
  iSum' <- new global_summaryval
  cw_len <- peek lencw
  c_sninit_ iPrt' iSum' cw lencw iw leniw rw lenrw (cw_len*8)
  free iPrt'
  free iSum'

snInit :: SnoptA -> IO ()
snInit sna = snInit' (sna ^. sna_ws)

setOptionI' :: Workspace -> String -> SnInteger -> IO ()
setOptionI' (Workspace cw iw rw lencw leniw lenrw) name value = do
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

setOptionI :: SnoptA -> String -> SnInteger -> IO ()
setOptionI sna = setOptionI' (sna ^. sna_ws)

snJac :: SnoptA -> IO (SnInteger,SnInteger,SnInteger,SnInteger)
snJac (SnoptA workspace
  (SnX { _sx_x = x, _sx_xlow = xlow, _sx_xupp = xupp, _sx_nx = nx, _sx_xstate = xstate, _sx_xmul = xmul})
  (SnF { _sf_nF = nF, _sf_fstate = fstate, _sf_objAdd = objAdd, _sf_objRow = objRow
       , _sf_flow = flow, _sf_fupp = fupp, _sf_f = f, _sf_fmul = fmul })
  (SnA {_sa_iAfun = iAfun, _sa_jAvar = jAvar, _sa_lenA = lenA, _sa_neA = neA, _sa_a = a})
  (SnG {_sg_iGfun = iGfun, _sg_jGvar = jGvar, _sg_lenG = lenG, _sg_neG = neG})
  userf ) = do
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

snopta :: SnoptA -> IO SnInteger
snopta (SnoptA workspace
  (SnX { _sx_x = x, _sx_xlow = xlow, _sx_xupp = xupp, _sx_nx = nx, _sx_xstate = xstate, _sx_xmul = xmul})
  (SnF { _sf_nF = nF, _sf_fstate = fstate, _sf_objAdd = objAdd, _sf_objRow = objRow
       , _sf_flow = flow, _sf_fupp = fupp, _sf_f = f, _sf_fmul = fmul })
  (SnA {_sa_iAfun = iAfun, _sa_jAvar = jAvar, _sa_lenA = lenA, _sa_neA = neA, _sa_a = a})
  (SnG {_sg_iGfun = iGfun, _sg_jGvar = jGvar, _sg_lenG = lenG, _sg_neG = neG})
  userf )
  = do
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
  userf' <- wrap userfg

  snopt <- mallocSnoptA 500 10000 20000 nx nf na ng userf'
  snInit snopt

  setXlow snopt xlow
  setXupp snopt xupp
  setX snopt xInit

  setFlow snopt flow
  setFupp snopt fupp
  setF snopt f0

  setObjRow snopt 1
  setObjAdd snopt 0

  setIAfun snopt iA
  setJAvar snopt jA
  setA snopt aval

  setIGfun snopt iG
  setJGvar snopt jG

  snopta snopt >>= (putStrLn . ("snopta: ret: " ++) . show)
