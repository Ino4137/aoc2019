module Main where

import System.Environment (getArgs)

import Inputs -- dayN
import TestInputs -- dayNtest
import Ex.E1
import Ex.E2

main :: IO ()
main = do
  (n:_) <- getArgs
  case n of
    "1p1" -> solve1p1 day1
    "1p2" -> solve1p2 day1
    "2p1" -> solve2p1 day2
    "2p2" -> solve2p2 day2
    _     -> error "undefined day - (day)p(part)"
