module DayFour where

import Util

type Range = (Int, Int)

parseRange :: String -> Range
parseRange = twoTuple . map (read :: String -> Int) . splitOn '-'

type RangePair = (Range, Range)

parseRangePair :: String -> RangePair
parseRangePair = twoTuple . map parseRange . splitOn ','


parse :: [String] -> [RangePair]
parse = map parseRangePair

rangesContained :: RangePair -> Bool
rangesContained ((a, b), (c, d)) = (a <= c && b >= d) || (c <= a && d >= b)

overlap :: RangePair -> Bool
overlap ((a, b), (c, d)) = go a b c d || go c d a b
  where go w x y z = (w >= y && w <= z) || (x >= y && x <= z)

dayFourMain :: [String] -> Int
dayFourMain = length . filter overlap . parse
