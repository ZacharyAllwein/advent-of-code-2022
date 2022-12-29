module DayFive where

import Util
import Data.Maybe
import Text.Read

type Box = [Char]

parseBoxes :: [String] -> [Box]
parseBoxes boxes =
  map (removeSpace . (flip vertSlice) rows)
      indexes
  where rows = take 8 boxes
        indexes = [(x * 4) + 1 | x <- [0..8]]

type Instruction = (Int, Int, Int)

parseInstructions :: [String] -> [Instruction]
parseInstructions = 
  map (threeTuple . 
       catMaybes .
       map readMaybe . 
       words)

executeInstruction :: Instruction -> [Box] -> [Box]
executeInstruction (n, a, b) boxes = go (zip boxes [1..(length boxes)])
  where ba = boxes !! (a - 1)
        na = drop n ba
        nb = (reverse $ take n ba) ++ (boxes !! ( b - 1))
        go [] = []
        go ((x, i): xs)
          | i == a = na : go xs
          | i == b = nb : go xs
          | otherwise = x : go xs

executeInstruction9001 :: Instruction -> [Box] -> [Box]
executeInstruction9001 (n, a, b) boxes = go (zip boxes [1..(length boxes)])
  where ba = boxes !! (a - 1)
        na = drop n ba
        nb = (take n ba) ++ (boxes !! ( b - 1))
        go [] = []
        go ((x, i): xs)
          | i == a = na : go xs
          | i == b = nb : go xs
          | otherwise = x : go xs



dayFiveMain :: [String] -> [Box]
dayFiveMain content = foldl (flip executeInstruction9001) 
                            (parseBoxes boxes)
                            (parseInstructions instructions)
  where (boxes, instructions) = twoTuple $ splitOn "" content


