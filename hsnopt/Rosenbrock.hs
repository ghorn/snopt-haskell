{-# OPTIONS_GHC -Wall #-}

module Main where

import Snopt.Nlp

--rosenbrock :: Nlp ()
--rosenbrock = do
--  x <- designVar "x"
--  y <- designVar "y"
--  1.3 <== x
----  x <== 1.3
--  minimize $ (1-x)**2 + 100*(y-x**2)**2

rosenbrock :: Nlp ()
rosenbrock = do
  x <- designVar "x"
--  1.3 <== x
--  x <== 0.5
  x <== (-0.1)
  minimize $ (x-0.7)**2

toy1 :: Nlp ()
toy1 = do
  x1 <- designVar "x1"
  x2 <- designVar "x2"

  x1**2 + 4*x2**2 <== 4
  (x1 - 2)**2 + x2**2 <== 5
  x1 <== 0

  minimize x2

doc :: Nlp ()
doc = do
  x1 <- designVar "x1"
  x2 <- designVar "x2"
  x3 <- designVar "x3"
  x4 <- designVar "x4"

  minimize $ 3*x1 + 5*x2 + (x1 + x3 + x4)**2
  x1 + x3**2 + x4**2 === 2
  0 <== 2*x3 + 4*x4
  x2 + x4**4 === 4

main :: IO ()
main = do
  (ret, logs) <- solveNlp doc
  putStrLn "\nsolution:"
  print ret
  putStrLn "\nlogs:"
  mapM_ print logs


--myNlp :: Floating a => Nlp a ()
--myNlp = do
--  x1 <- designVar "x1"
--  x2 <- designVar "x2"
--
----  x1 === x2
--
--  x1**2 + 4*x2**2 <== 4
--  (x1-2)**2 + x2**2 <== 5
--  0 <== x1
----  leq3 (-500) x2 500
--
--  minimize x2
--
--main :: IO ()
--main = solveNlp myNlp
