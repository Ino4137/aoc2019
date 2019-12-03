module Ex.E2 (solve2p1, solve2p2) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Recursion
import           Data.List.Split
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

parse :: String -> Vector Int
parse = V.fromList . map read . splitOn ","

type Carry = (Int, Vector Int)
solve2p1 = print . fst . solve2p1' 12 2 . parse

solve2p2 (parse -> m) = print . uncurry (+) . first (*100)
  . snd . head . dropWhile ((/= 19690720).fst) $
  [0..99] >>= traverse ((flip.(flip.) $ solve2p1') m) [0..99]

solve2p1' noun verb = (,(noun,verb)) . V.head . snd . last . ana whileNot99 . (0,)
  . (ix 2 .~ verb) . (ix 1 .~ noun)
  where
    whileNot99 :: Carry -> ListF Carry Carry
    whileNot99 (i, x) =
      let a = (x!(x!(i+1)))
          b = (x!(x!(i+2)))
      in foo (\m -> let c = (i+4, x & ix (x!(i+3)) .~ m) in Cons c c) a b
      where
        foo f a b = case x ! i of
          99 -> Nil
          1  -> f (a + b)
          2  -> f (a * b)
          _  -> undefined
