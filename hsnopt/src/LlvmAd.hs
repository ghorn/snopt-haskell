{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}

module LlvmAd ( doMagic
              , mkIOStub3
              ) where

import Data.Either ( partitionEithers )
import Data.Hashable ( Hashable )
import Foreign.Ptr ( Ptr, FunPtr )
import qualified Data.Vector as V
import GHC.Word ( Word32 )
import Numeric.AD ( jacobian' )

import Snopt

import qualified Dvda
import Dvda.Expr
import Dvda.FunGraph ( FunGraph)
import Dvda.Llvm

foreign import ccall "dynamic" mkIOStub3 ::
  FunPtr (Ptr Double -> Ptr Double -> Ptr Double -> IO Word32)
  -> Ptr Double -> Ptr Double -> Ptr Double -> IO Word32

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

doMagic :: (Show a, Hashable a, Ord a, Real a, Fractional a, Floating a) =>
           (forall s. Floating s => [s] -> [s])
        -> [Expr a]
        -> IO ( FunGraph VList VList a
              , [((SnInteger,SnInteger), a)]
              , [(SnInteger,SnInteger)]
              )
doMagic f x = do
  let (f0,jac) = unzip $ jacobian' f x
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

  putStrLn "--------- f ---------"
  print f0
  putStrLn "--------- jacobian f ---------"
  mapM_ print jac
  putStrLn "--------- nonlinaer jacobian f ---------"
  mapM_ print nljac
  putStrLn "--------- linaer jacobian f ---------"
  mapM_ print linjac
  fg <- Dvda.toFunGraph
        (VList (V.singleton (V.fromList x)))
        (VList (V.fromList [V.fromList f0, V.fromList g]))

  return (fg, ijxA, ijG)
