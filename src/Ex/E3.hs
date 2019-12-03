module Ex.E3 (solve3p1, solve3p2) where

import           Control.Recursion
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List.Split
import qualified Data.Map as M
import           Data.Semigroup
import qualified Data.Set as S

data Dir = D Int | L Int | U Int | R Int 

parse :: String -> [[Dir]]
parse = map (map pparser . splitOn ",") . lines
  where
    pparser (d:p) = read p & case d of
      'D' -> D
      'U' -> U
      'R' -> R
      'L' -> L

solve3p1 = print . foldMap (Min . manhattan) . solve3p1'
  where
    manhattan (a,b) = abs a + abs b

solve3p1' (parse -> [a, b]) =
  cata walk a (0,0) `S.intersection` cata walk b (0,0)
  where
    walk Nil _ = S.empty
    walk (Cons d r) (!x,!y) = case d of
      D a -> (r (x,y-a) `S.union`) .
        S.fromDescList $ [1..a] <&> (x,) . (y-)
      U a -> (r (x,y+a) `S.union`) .
        S.fromAscList $ [1..a] <&> (x,) . (y+)
      L a -> (r (x-a,y) `S.union`) .
        S.fromDescList $ [1..a] <&> (,y) . (x-)
      R a -> (r (x+a,y) `S.union`) .
        S.fromAscList $ [1..a] <&> (,y) . (x+)

transform (D n) = replicate n (D 0)
transform (U n) = replicate n (U 0)
transform (L n) = replicate n (L 0)
transform (R n) = replicate n (R 0)

solve3p2 i@(parse -> [(>>= transform) -> a, (>>= transform) -> b]) =
  print . fold . map (Min . snd) . M.toList
  $ M.unionWith (+) (cata walk a (0, (0,0))) (cata walk b (0, (0,0)))
  where
    crosses = solve3p1' i
    walk Nil _ = M.empty
    walk (Cons d r) (!s,(!x,!y)) =
      let sn = s+1 :: Int
          p = case d of
            D _ -> (x,y-1)
            U _ -> (x,y+1)
            R _ -> (x+1,y)
            L _ -> (x-1,y)
      in if S.member p crosses then
        M.singleton p sn <> r (sn, p)
      else r (sn,p)
