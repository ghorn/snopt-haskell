## Haskell bindings to SNOPT (Sparse Nonlinear OPTimizer) verson 7

Directories:
+ snopt-base: direct bindings to the fortran subroutines
+ snopt: high(er)-level bindings

This is a work in progress, and is broken except on my system at the moment. Known issues:
- some paths in snopt-base.cabal are hardcoded for my system
- some libraries are specific to the Intel fortran compiler
- to avoid name conflicts with hmatrix, you must rename SNOPT's libblas.{a,so} to libsnopt_blas.{a,so}
