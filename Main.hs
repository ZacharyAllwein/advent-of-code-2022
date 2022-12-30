module Main where

import DaySeven


main :: IO ()
main = do
  content <- readFile "data.txt"
  testContent <- readFile "sample.txt"
  print (daySevenMain (lines content))
  print (daySevenMain (lines testContent))

