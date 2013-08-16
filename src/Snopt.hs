{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C ( CInt(..), CDouble(..), CChar(..) )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Storable

type SnInteger = Int
type SnDoubleReal = CDouble
type SnChar = CChar
type SnFtnLen = Int
type SnInt = CInt

type SnInit_ = Ptr SnInteger -- ( integer *iPrint
               -> Ptr SnInteger -- , integer *iSumm
               -> Ptr SnChar -- , char *cw
               -> Ptr SnInteger -- , integer *lencw
               -> Ptr SnInteger -- , integer *iw
               -> Ptr SnInteger -- , integer *leniw
               -> Ptr SnDoubleReal -- , doublereal *rw
               -> Ptr SnInteger -- , integer *lenrw
--               -> SnFtnLen -- , ftnlen cw_len)
               -> IO () -- SnInt -- extern int sninit_
foreign import ccall unsafe "sninit_" c_sninit_ :: SnInit_

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
--              -> SnFtnLen -- , ftnlen cu_len
--              -> SnFtnLen -- , ftnlen cw_len);
              -> IO () -- extern int snjac_
foreign import ccall unsafe "snjac_" c_snjac_ :: SnJac_

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

userf' :: U_fp
userf' _ _ x _ _ f _ _ _ _ _ _ _ _ _ = do
  x0 <- peekElemOff x 0
  x1 <- peekElemOff x 1
  x2 <- peekElemOff x 2
  pokeElemOff f 0 (x1)
  pokeElemOff f 1 (x0*x0 + 4*x1*x1)
  pokeElemOff f 2 ((x0 - 2)*(x0 - 2) + x1*x1)

toy0 = undefined
  where
    nF = 3
    objRow = 1
    n = 2
    objAdd = 0
    xlow  = [    0, -1e12]
    xhigh = [ 1e12,  1e12]
    fstate = [0,0,0]
    flow = [-1e12,-1e12,-1e12]
    fupp = [ 1e12, 4, 5]
    fmul = [0,0,0]
    f = [0,0,0]
    x = [1,1]
    prob = "sntoy1"
    

main :: IO ()
main = do
  let lencw' = 500
      leniw' = 10000
      lenrw' = 20000
  iSumm <- new 6
  iPrint <- new 0
  lencw <- new lencw'
  leniw <- new leniw'
  lenrw <- new lenrw'
  cw <- mallocArray lencw'
  iw <- mallocArray leniw'
  rw <- mallocArray lenrw'
--  let cw_len = 8*n
  c_sninit_ iPrint iSumm cw lencw iw leniw rw lenrw -- cw_len


  nF <- new 3
  objRow <- new (1::SnInteger)
  n <- new 2
  objAdd <- new (0::SnDoubleReal)
  xlow <- newArray [    0, -1e12]
  xupp <- newArray [ 1e12,  1e12]
  fstate <- newArray [0,0,0::SnDoubleReal]
  flow <- newArray [-1e12,-1e12,-1e12::SnDoubleReal]
  fupp <- newArray [ 1e12, 4, 5::SnDoubleReal]
  fmul <- newArray [0,0,0::SnDoubleReal]
  f <- newArray [0,0,0::SnDoubleReal]
  x <- newArray [1,1]
  prob <- newArray "sntoy1"

  userf <- wrap userf'

  let cu = cw
      iu = iw
      ru = rw
      lencu = lencw
      leniu = leniw
      lenru = lenrw
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
  a <- mallocArray lenA'
  iAfun <- mallocArray lenA'
  jAvar <- mallocArray lenA'
  jGvar <- mallocArray lenG'
  iGfun <- mallocArray lenG'
  c_snjac_ info nF n userf
    iAfun jAvar lenA neA a
    iGfun jGvar lenG neG
    x xlow xupp mincw miniw minrw
    cu lencu iu leniu ru lenru
    cw lencw iw leniw rw lenrw






--foreign import ccall unsafe "cplex.h CPXopenCPLEX" c_CPXopenCPLEX :: Ptr CInt -> IO (Ptr CPXENV)
--foreign import ccall unsafe "cplex.h CPXcloseCPLEX" c_CPXcloseCPLEX :: Ptr (Ptr CPXENV) -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXcreateprob" c_CPXcreateprob ::
--  Ptr CPXENV -> Ptr CInt -> Ptr CChar -> IO (Ptr CPXLP)
--foreign import ccall unsafe "cplex.h CPXfreeprob" c_CPXfreeprob ::
--  Ptr CPXENV -> Ptr (Ptr CPXLP) -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXnewrows" c_CPXnewrows ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt -> Ptr CDouble -> Ptr CChar -> Ptr CDouble -> Ptr (Ptr CChar) -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXaddrows" c_CPXaddrows ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr (Ptr CChar) -> Ptr (Ptr CChar) -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXnewcols" c_CPXnewcols ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr (Ptr CChar) -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXaddcols" c_CPXaddcols ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr (Ptr CChar) -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXchgcoeflist" c_CPXchgcoeflist ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXchgcoef" c_CPXchgcoef ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt -> CInt -> CDouble -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXgeterrorstring" c_CPXgeterrorstring ::
--  Ptr CPXENV -> CInt -> Ptr CChar -> IO ()
--
--foreign import ccall unsafe "cplex.h CPXgetstatstring" c_CPXgetstatstring ::
--  Ptr CPXENV -> CInt -> Ptr CChar -> IO ()
--
--foreign import ccall unsafe "cplex.h CPXsetintparam" c_CPXsetintparam ::
--  Ptr CPXENV -> CInt -> CInt -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXgetnumcols" c_CPXgetnumcols ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt
--
--foreign import ccall unsafe "cplex.h CPXgetnumrows" c_CPXgetnumrows ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt
--
--foreign import ccall unsafe "cplex.h CPXcopylp" c_CPXcopylp ::
--  Ptr CPXENV -> Ptr CPXLP -> CInt -> -- (CPXCENVptr env, CPXLPptr lp, int numcols,
--  CInt -> CInt -> Ptr CDouble ->     --  int numrows, int objsense, const double *objective,
--  Ptr CDouble -> Ptr CChar ->        --  const double *rhs, const char *sense,
--  Ptr CInt -> Ptr CInt ->            --  const int *matbeg, const int *matcnt,
--  Ptr CInt -> Ptr CDouble ->         --  const int *matind, const double *matval,
--  Ptr CDouble -> Ptr CDouble ->      --  const double *lb, const double *ub,
--  Ptr CDouble                        --  const double *rngval);
--  -> IO CInt
--
--
--foreign import ccall unsafe "cplex.h CPXcopyquad" c_CPXcopyquad ::
--  Ptr CPXENV -> Ptr CPXLP -> Ptr CInt -> -- (CPXCENVptr env, CPXLPptr lp, const int *qmatbeg, 
--  Ptr CInt -> Ptr CInt ->                --  const int *qmatcnt, const int *qmatind,
--  Ptr CDouble ->                         --  const double *qmatval);
--  IO CInt
--
--foreign import ccall unsafe "cplex.h CPXqpopt" c_CPXqpopt ::
--  Ptr CPXENV -> Ptr CPXLP -> IO CInt
--
--foreign import ccall unsafe "cplex.h CPXsolution" c_CPXsolution ::
--  Ptr CPXENV -> Ptr CPXLP -> Ptr CInt ->       -- (CPXCENVptr env, CPXCLPptr lp, int *lpstat_p,
--  Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> --  double *objval_p, double *x, double *pi,
--  Ptr CDouble -> Ptr CDouble -> IO CInt        --  double *slack, double *dj);
