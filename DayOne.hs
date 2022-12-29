module DayOne where

import Util


dayOneMain :: [String] -> Int
dayOneMain = sum . foldr replaceBig [0, 0, 0] . toTotCals . toLInts

replaceBig :: Int -> [Int] -> [Int]
replaceBig a bs =
  if a > small
  then
    let
      (cs, ds) = break (==small) bs 
    in cs ++ [a] ++ (tail ds)
  else bs
  where small = minimum bs

maxCals :: [Int] -> Int
maxCals = foldr max 0

toTotCals :: [[Int]] -> [Int]
toTotCals = map sum

toLInts :: [String] -> [[Int]]
toLInts = (map . map) (read :: String -> Int) . uSplit ""
