module Main where

import DaySix


main :: IO ()
main = do
  content <- readFile "data.txt"
  testContent <- readFile "sample.txt"
  print (daySixMain ( content))
  print (daySixMain (testContent))

