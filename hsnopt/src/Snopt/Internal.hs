{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}

module Snopt.Internal ( -- * types
                        SnoptA'
                      , mallocSnoptA
                      , freeSnoptA
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

                      , sna_ws
                      , sna_x
                      , sna_f
                      , sna_a
                      , sna_g
                      , sna_iprint
                      , sna_isummary
                      , sna_ufp
                      ) where

import Control.Lens ( makeLenses )
import Control.Lens.Getter
import Foreign.C.String
import Foreign.Ptr ( FunPtr, Ptr )
import Foreign.Marshal
import Foreign.Storable

import Snopt.Bindings

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
data SnoptA' = SnoptA' { _sna_ws :: Workspace
                       , _sna_x :: SnX
                       , _sna_f :: SnF
                       , _sna_a :: SnA
                       , _sna_g :: SnG
                       , _sna_iprint :: Ptr SnInteger
                       , _sna_isummary :: Ptr SnInteger
                       , _sna_ufp :: FunPtr U_fp
                       }
makeLenses ''SnX
makeLenses ''SnF
makeLenses ''SnA
makeLenses ''SnG
makeLenses ''SnoptA'

---------------------------------- malloc/free ------------------------------
mallocSnoptA :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> FunPtr U_fp
                -> IO (Either String SnoptA')
mallocSnoptA nc ni nr nx nf na ng ufp = do
  sag <- mallocAG na ng
  case sag of
    Left e -> return (Left e)
    Right (sa,sg) -> do
      workspace <- mallocWorkspace nc ni nr
      sx <- mallocX nx
      sf <- mallocF nf
      iprint <- new 0
      isum <- new 6
      return $ Right $ SnoptA' workspace sx sf sa sg iprint isum ufp
  
freeSnoptA :: SnoptA' -> IO ()
freeSnoptA (SnoptA' ws x f a g iprint isum _) = do
  freeWorkspace ws
  freeX x
  freeF f
  free iprint
  free isum
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

mallocAG :: Int -> Int -> IO (Either String (SnA, SnG))
mallocAG neA neG
  | neA < 0 = return (Left "mallocAG: neA < 0")
  | neG < 0 = return (Left "mallocAG: neG < 0")
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
  return (Right (sna, sng))

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

--------------------------------------------------------------------------------------
sninit' :: Workspace -> Ptr SnInteger -> Ptr SnInteger -> IO ()
sninit' (Workspace cw iw rw lencw leniw lenrw) iPrt iSum = do
  cw_len <- peek lencw
  c_sninit_ iPrt iSum cw lencw iw leniw rw lenrw (cw_len*8)

sninit :: SnoptA' -> IO ()
sninit sna = sninit' (sna ^. sna_ws ) (sna ^. sna_iprint) (sna ^. sna_isummary)

snseti' :: Workspace -> Ptr SnInteger -> Ptr SnInteger -> String -> SnInteger -> IO SnInteger
snseti' (Workspace cw iw rw lencw leniw lenrw) iPrt iSum name value = do
  errors' <- new 0
  (name',bufferlen) <- newCStringLen name
  value' <- new value

  cw_len <- peek lencw
  c_snseti_
    name' value' iPrt iSum errors'
    cw lencw iw leniw rw lenrw
    (fromIntegral bufferlen) (8*cw_len)
  errors <- peek errors'

  free errors'
  free name'
  free value'

  return errors

snseti :: SnoptA' -> String -> SnInteger -> IO SnInteger
snseti sna = snseti' (sna ^. sna_ws) (sna ^. sna_iprint) (sna ^. sna_isummary)

snjac :: SnoptA' -> IO (SnInteger,SnInteger,SnInteger,SnInteger)
snjac (SnoptA' workspace
  (SnX { _sx_x = x, _sx_xlow = xlow, _sx_xupp = xupp, _sx_nx = nx })
  (SnF { _sf_nF = nF })
  (SnA { _sa_iAfun = iAfun, _sa_jAvar = jAvar, _sa_lenA = lenA, _sa_neA = neA, _sa_a = a })
  (SnG { _sg_iGfun = iGfun, _sg_jGvar = jGvar, _sg_lenG = lenG, _sg_neG = neG })
  _ _
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

  return (info', mincw', miniw', minrw')

snopta :: SnoptA' -> String -> IO SnInteger
snopta (SnoptA' workspace
  (SnX { _sx_x = x, _sx_xlow = xlow, _sx_xupp = xupp, _sx_nx = nx, _sx_xstate = xstate, _sx_xmul = xmul})
  (SnF { _sf_nF = nF, _sf_fstate = fstate, _sf_objAdd = objAdd, _sf_objRow = objRow
       , _sf_flow = flow, _sf_fupp = fupp, _sf_f = f, _sf_fmul = fmul })
  (SnA {_sa_iAfun = iAfun, _sa_jAvar = jAvar, _sa_lenA = lenA, _sa_neA = neA, _sa_a = a})
  (SnG {_sg_iGfun = iGfun, _sg_jGvar = jGvar, _sg_lenG = lenG, _sg_neG = neG})
  _ _
  userf ) problemName
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

    (probstr,prob_len) <- newCStringLen (problemName ++ "        ")

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

