{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Snopt.Bindings ( SnInteger, SnDoubleReal, SnChar, SnFtnLen, U_fp
                      , c_sninit_
                      , c_snjac_
                      , c_snopta_
                      , c_snseti_
                      , c_snsetr_
                      , wrap
                      ) where

import Foreign.C ( CInt(..), CChar(..) )
import Foreign.Ptr ( Ptr, FunPtr )

type SnInteger = CInt
type SnDoubleReal = Double -- CDouble
type SnChar = CChar
type SnFtnLen = CInt
type SnRet = ()

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
               -> IO SnRet -- extern int snopta_
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
               -> IO SnRet -- SnInt -- extern int sninit_
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
                -> IO SnRet -- extern int snseti_

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
              -> IO SnRet -- extern int snjac_
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
            -> IO SnRet -- int toyusrf_

foreign import ccall "wrapper"
  wrap :: U_fp -> IO (FunPtr U_fp)
