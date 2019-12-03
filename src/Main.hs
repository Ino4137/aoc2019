module Main where

import System.Environment (getArgs)

import Inputs -- dayN
import TestInputs -- dayNtest
import Ex.E1
import Ex.E2
import Ex.E3

main :: IO ()
main = do
  (n:_) <- getArgs
  case n of
    "1p1" -> solve1p1 day1
    "1p2" -> solve1p2 day1
    "2p1" -> solve2p1 day2
    "2p2" -> solve2p2 day2
    "3p1" -> solve3p1 day3
    "3p2" -> solve3p2 day3
    _     -> error "undefined day - (day)p(part)"
