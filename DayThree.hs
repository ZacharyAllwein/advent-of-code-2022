module DayThree where

import Util
import qualified Data.Map as M
import Data.Maybe

type Rucksack = (String, String)

makeRucksack :: String -> Rucksack
makeRucksack str = splitAt (div (length str) 2) str

disjRucksack :: Rucksack -> Char
disjRucksack (as, bs) = head $ filter (flip elem bs) as

disj :: (Eq a) => [[a]] -> [a]
disj (a:as) = foldr (filter . (flip elem)) a as

priorityMap :: M.Map Char Int
priorityMap =
  M.fromList (zip (['a'..'z'] ++ ['A'..'Z']) [1..52])

dayThreeMain :: [String] -> Int
dayThreeMain = 
  sum 
  . catMaybes
  . map ((flip M.lookup) priorityMap . disjRucksack . makeRucksack)

dayThreeMain2 :: [String] -> Int
dayThreeMain2 = 
  sum 
  . catMaybes 
  . map ((flip M.lookup) priorityMap . head . disj) . uWindows 3 
