module Ex.E4 (solve4p1, solve4p2) where

import Data.Bifunctor
import Data.Char
import Data.List (group, sort)

solveP cond = print . length . filter cond . filter ((==) . reverse <*> sort)
  . map digitize . uncurry enumFromTo . prep
  where
    prep = bimap read read . fmap tail . break (== '-')
    digitize = map snd . takeWhile (/= (0,0)) .
      iterate (flip quotRem 10 . fst) . flip quotRem 10 
        
solve4p1 = solveP (any ((> 1) . length) . group)

solve4p2 = solveP (any ((== 2) . length) . group) 
