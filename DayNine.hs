module DayNine where

import Data.Bifunctor
import Data.List
import Util

type Pos = (Int, Int)

type Rope = (Pos, Pos)

type Rope2 = [Pos]

data Direction =
    U
  | D
  | L
  | R
  deriving (Eq, Show, Read)

updatePos :: Direction -> Pos -> Pos
updatePos U = first (subtract 1)
updatePos D = first (+1)
updatePos L = second (subtract 1)
updatePos R = second (+1)

closerPos :: Pos -> Pos -> Pos
closerPos (a, b) (c, d) = (closer a c, closer b d)

isAdjacent :: Rope -> Bool
isAdjacent ((a, b), (c, d)) = abs (a - c) <= 1 && abs (b - d) <= 1

updateTail :: Rope -> Rope
updateTail r@((a, b), (c, d)) = 
  if isAdjacent r
  then r
  else ((a, b), (uncurry (flip closerPos)) r)

updateHead :: Direction -> Rope -> Rope
updateHead = first . updatePos

updateRope :: Direction -> Rope -> Rope
updateRope = (updateTail .) . updateHead

updateRope2 :: Direction -> Rope2 -> Rope2
updateRope2 a (b:bs) = 
  scanl 
    (\x y -> if isAdjacent (x, y) then y else closerPos y x) 
    (updatePos a b) 
    bs

states :: [Direction] -> Rope -> [Rope]
states = flip $ scanl (flip updateRope)

states2 :: [Direction] -> Rope2 -> [Rope2]
states2 = flip $ scanl (flip updateRope2)

rope :: Rope
rope = ((0, 0), (0, 0))

rope2 :: Rope2
rope2 = take 10 (repeat (0, 0))

parseUnit :: String -> [Direction]
parseUnit = 
    (uncurry $ flip replicate) 
  . bimap (read :: String -> Direction) (read :: String -> Int)
  . twoTuple 
  . words 

parse :: [String] -> [Direction]
parse = (flip (>>=)) parseUnit

dayNineMain :: [String] -> Int
dayNineMain x = length . nub . map last $ states2 (parse x) rope2

