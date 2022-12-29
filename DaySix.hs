module DaySix where
import Util

daySixMain :: String -> Int
daySixMain s = (+14) $ snd $ head $ filter (allUnique . fst) $ zip (sequences 14 s) [0..]

