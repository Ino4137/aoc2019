module Ex.E1 (solve1p1, solve1p2) where

solve1p1 = print . sum . map (subtract 2 . floor . (/3) . read) . lines
solve1p2 = print . sum
  . map (sum . takeWhile (>= 0) . tail . iterate f . read) . lines
  where
    f = subtract 2 . floor . (/3) . fromIntegral
