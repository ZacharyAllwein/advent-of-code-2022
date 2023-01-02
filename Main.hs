module Main where

import DayNine


main :: IO ()
main = do
  content <- readFile "data.txt"
  testContent <- readFile "sample.txt"
  print (dayNineMain (lines content))
  print (dayNineMain (lines testContent))

