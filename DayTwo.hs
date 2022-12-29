module DayTwo where

import Util

data RPS = R | P | S deriving (Eq, Show)

readRPS :: String -> RPS
readRPS "A" = R
readRPS "B" = P
readRPS "C" = S
readRPS "X" = R
readRPS "Y" = P
readRPS "Z" = S

makeRPSTup :: (String, String) -> (RPS, RPS)
makeRPSTup (a, b) = (c, d)
  where
    c = readRPS a
    d = case b of
      "X" -> (toWin . toWin) c
      "Y" -> c
      "Z" -> toWin c 

scoreChoice :: RPS -> Int
scoreChoice R = 1
scoreChoice P = 2
scoreChoice S = 3

toWin :: RPS -> RPS
toWin R = P
toWin P = S
toWin S = R

isWin :: RPS -> RPS -> Bool
isWin a b = b == (toWin a)

winPoints :: (RPS, RPS) -> Int
winPoints (a, b)
  | a == b = 3
  | isWin a b = 6
  | otherwise = 0

score :: (RPS, RPS) -> Int
score (a, b) = (scoreChoice b) + (winPoints (a, b))

parse :: [String] -> [(RPS, RPS)]
parse = map (twoTuple . (map readRPS . uSplit ' '))

parse2 = map (makeRPSTup . twoTuple . uSplit ' ')

dayTwoMain :: [String] -> Int
dayTwoMain = sum . (map score) . parse2
