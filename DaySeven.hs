module DaySeven where

import Util
import Data.List
import Data.Maybe
import Data.Either

--name, size

type Path = [String]

type File = (String, Int)

--file system type product of directory and path
type FS = (Dir, Path)

type LsOut = Either Dir File

data Command = CD String | LS [LsOut] deriving (Eq, Show)

data Dir = 
  Dir {name :: String, children :: [Dir], files :: [File]}
  deriving (Show, Eq)

parseLsOut :: String -> LsOut
parseLsOut s
  | take 3 s == "dir" = Left $ Dir (last $ words s) [] []
  | otherwise = Right (name, (read size :: Int))
    where (size, name) = twoTuple $ words s

parseCommand :: [String] -> Command
parseCommand ss = case take 3 $ head ss of
  "$ l" -> LS (map parseLsOut (tail ss))
  "$ c" -> CD $ last $ words $ head ss

parse :: [String] -> [Command]
parse = map parseCommand . breakAll ((=='$') . head)

--finally a function capable of getting shit done
--takes a mapping function and the current dir and changes the directory at the right path
--iterates through path, traversing through directories and rebuilding them
--kind of like map with a predicate but the predicate is the path of the file
changeAt :: (Dir -> Dir) -> Path -> Dir -> Dir
changeAt f (p:ps) dir
      | p == name dir = case ps of
        [] -> f dir
        _ -> dir { children = map (changeAt f ps) (children dir) }
      | otherwise = dir

dirFlatMap :: (Dir -> a) -> Dir -> [a]
dirFlatMap f dir = case children dir of
  [] -> [f dir]
  childs -> [f dir] ++ (concat $ map (dirFlatMap f) (children dir))

totalSize :: Dir -> Int
totalSize dir = case children dir of
  [] -> (sum $ (map snd (files dir)))
  xs -> (sum $ (map snd (files dir))) + (sum $ map totalSize (children dir))

--takes a name and a dir and if a dir with that name exists it will be returned useful for checking things maybe
subDir :: String -> Dir -> Maybe Dir
subDir n = listToMaybe . filter ((==n) . name) . children

--takes a dir and puts it into another dir
insertDir :: Dir -> Dir -> Dir
insertDir child par = par { children = child : children par }

--takes a file and puts it into dir
insertFile :: File -> Dir -> Dir
insertFile file dir = dir { files = file : files dir }


--shows us the directory we are currently at on the file system
current :: FS -> Dir
current (dir, _:path) = foldl (\x y -> fromJust $ subDir y x) dir path

--mkdir Path sensitive, takes a string and puts it in the FS at the current path
mkdir :: Dir -> FS -> FS
mkdir d (dir, path) = (changeAt (insertDir d) path dir, path)

--mkfile like mkdir but for files, wow this is really repetitive could somebody
--invent HOFs or something
mkfile :: File -> FS -> FS
mkfile file (dir, path) = (changeAt (insertFile file) path dir, path)

--changes path but does so correctly
cd :: String -> FS -> FS
cd ".." (dir, path)
  | length path == 1 = (dir, path)
  | otherwise = (dir, take ((length path) - 1) path)
cd "/" (dir, _) = (dir, ["/"])
cd s (dir, path)
  | elem s (map name $ children $ current (dir, path)) = (dir, path ++ [s])
  | otherwise = (dir, path)

applyCommand :: Command -> FS -> FS
applyCommand (CD s) fs = cd s fs
applyCommand (LS lso) fs = 
  foldl
    (flip mkdir)
    (foldl (flip mkfile) fs (rights lso))
    (lefts lso)

readParsed :: [Command] -> FS -> FS
readParsed coms fs = foldl (flip applyCommand) fs coms

--all hail root
root :: Dir
root = Dir "/" [] []

--starting system before we do anything
fs :: FS
fs = (root, ["/"])


daySevenMain :: [String] -> Int
daySevenMain ss = 
    minimum 
  $ filter (>=(30000000 - (70000000 - totalSize dir))) 
  $ (dirFlatMap totalSize) dir
  where (dir, path) = (flip readParsed) fs $ parse ss
