module Main where

import System.Environment (getArgs)

import Inputs -- dayN
import TestInputs -- dayNtest
import Ex.E1

main :: IO ()
main = do
  (n:_) <- getArgs
  case n of
    "1p1" -> solve1p1 day1
    "1p2" -> solve1p2 day1
    _     -> error "undefined day - (day)p(part)"
