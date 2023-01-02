module DayEight where

import Util
import Data.Bool

parse :: [String] -> [[Int]]
parse = (map . map) $ (read :: String -> Int) . (:[])

visible :: Ord a => a -> [a] -> Bool
visible _ [] = True
visible a bs = a > maximum bs

score :: Ord a => a -> [a] -> Int
score _ [] = 0
score _ (x:[]) = 1
score a (x:xs)
  | a <= x = 1
  | otherwise = 1 + score a xs

visibleAny :: Ord a => a -> [[a]] -> Bool
visibleAny = any . visible

scoreAll :: Ord a => a -> [[a]] -> Int
scoreAll e [as, bs, cs, ds] = 
  foldl (*) 1 $ map (score e) [reverse as, bs, reverse cs, ds]

dirSplit :: Int -> [a] -> [[a]]
dirSplit = ((twoList . fmap (drop 1)) .) . splitAt

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

distribute :: (a, [(b, c)]) -> [(a, b, c)]
distribute (a, bs) = map (uncurry ((,,) a)) bs

enum2D :: [[a]] -> [(Int, Int, a)]
enum2D = (flip (>>=)) (distribute . fmap enumerate) . enumerate

directions :: (Int, Int, a) -> [[a]] -> (a, [[a]])
directions (i, j, b) xs = 
  (b,
   dirSplit i (vertSlice j xs) ++ dirSplit j (xs !! i))


dayEightMain :: [String] -> Int
dayEightMain ss = maximum $ map (uncurry scoreAll) $ map ((flip directions) x) $ enum2D x
  where x = parse ss
