module Main where

import DayEight


main :: IO ()
main = do
  content <- readFile "data.txt"
  testContent <- readFile "sample.txt"
  print (dayEightMain (lines content))
  print (dayEightMain (lines testContent))

