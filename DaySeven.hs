module DaySeven where

import Data.List
import Data.Maybe

--name, size

type Path = [String]

type File = (String, Int)

data Dir = 
  Dir {name :: String, children :: [Dir], files :: [File]}
  deriving (Show, Eq)

changeAt :: (Dir -> Dir) -> Path -> Dir -> Dir
changeAt f (p:ps) dir
      | p == name dir = case ps of
        [] -> f dir
        _ -> dir { children = map (changeAt f ps) (children dir) }
      | otherwise = dir

subDir :: String -> Dir -> Maybe Dir
subDir n = listToMaybe . filter ((==n) . name) . children

popChild :: String -> Dir -> (Maybe Dir, [Dir]) 
popChild n dir = (listToMaybe match, diff)
  where (match, diff) = partition ((==n) . name) $ children dir

insertDir :: Dir -> Dir -> Dir
insertDir child par = par { children = child : children par }

root :: Dir
root = Dir "/" [] []


type FS = (Dir, Path)

mkdir :: String -> FS -> FS
mkdir s (dir, path) = (changeAt (insertDir (Dir s [] [])) path dir, path)

pathsTo :: Path -> [Path]
pathsTo = tail . map reverse . scanl (flip (:)) []

fs :: FS
fs = (root, ["/"])

test = mkdir "hello" fs

test2 = mkdir "world" test
