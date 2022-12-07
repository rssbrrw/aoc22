module Main
  ( main
  ) where

import           Data.Foldable   (toList)
import           Data.List       (foldl, sort)
import           Data.List.Split (splitOn)
import qualified Data.Map        as M (Map, empty, insertWith)

import           Lib             (runDay)

main = runDay partOne partTwo

partOne = sum . filter (<= 100000) . files

partTwo input = head $ sort $ filter (>= diff) $ list
  where
    list = files input
    diff = 30000000 - (70000000 - (head list))

files = toList . build M.empty ["/"] . tail

build m _ [] = m
build m path (x:xs) =
  case splitOn " " x of
    [_, "cd", ".."] -> build m (tail path) xs
    [_, "cd", dir] -> build m (((head path) ++ dir) : path) xs
    [_, "ls"] -> build m path xs
    ["dir", _] -> build m path xs
    [num, _] -> build (incAll m path size) path xs
      where size = read num
            incAll m paths x = foldl (insertWith (+) x) m paths
            insertWith op a b c = M.insertWith op c a b
