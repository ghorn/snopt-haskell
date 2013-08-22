{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}

module Snopt.AD ( makeFG
                ) where

import Data.Either ( partitionEithers )
import Numeric.AD ( jacobian' )

import Dvda.Expr

data WhoAmI a = Nonlinear (Expr a)
              | Linear a
              | Zero
whoAmI :: (Eq a, Num a) => Expr a -> WhoAmI a
whoAmI (EConst 0) = Zero
whoAmI (EConst c) = Linear c
whoAmI (ENum (FromInteger 0)) = Zero
whoAmI (ENum (FromInteger c)) = Linear (fromInteger c)
whoAmI (EFractional (FromRational 0)) = Zero
whoAmI (EFractional (FromRational c)) = Linear (fromRational c)
whoAmI x = Nonlinear x

makeFG :: (Real a, Floating a, Integral k) =>
          (forall s. Floating s => [s] -> [s]) -> [Expr a] -> ([Expr a], [Expr a], [((k,k), a)], [(k,k)])
makeFG f x = (f0, g, ijxA, ijG)
  where
    (f0,jac) = unzip $ jacobian' f x
    -- snopt is 1-indexed
    indices = [[(i,j) | j <- take (length x) [1..]] | i <- take (length f0) [1..]]

    splitLinear (x':xs) (ij:ijs) = case whoAmI x' of
      Zero        -> splitLinear xs ijs
      Nonlinear e -> Left  (ij,e):splitLinear xs ijs
      Linear c    -> Right (ij,realToFrac c):splitLinear xs ijs
    splitLinear [] [] = []
    splitLinear [] (_:_) = error "splitLinear: list mismatch"
    splitLinear (_:_) [] = error "splitLinear: list mismatch"

    (nljac, linjac) = partitionEithers $ splitLinear (concat jac) (concat indices)
    (ijG,g) = unzip nljac
    ijxA = linjac
