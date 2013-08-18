{-# OPTIONS_GHC -Wall #-}

module Main where

import Nlp

main :: IO ()
main = solveNlp $ do
  x <- designVar "x"
  y <- designVar "y"
  minimize $ (1-x)**2 + 100*(y-x**2)**2



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
