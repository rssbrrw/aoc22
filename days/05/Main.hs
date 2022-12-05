module Main (main) where

import Data.Char (isAlpha, isDigit)
import Data.Foldable (toList)
import Data.List (isPrefixOf, transpose)
import Data.List.Split (splitOn)
import qualified Data.Sequence as S (fromList, adjust', index)

import Lib (runDay)

main = runDay partOne partTwo

partOne = map head . moveCrates reverse
partTwo = map head . moveCrates id

moveCrates op input = toList $ foldl (move op) (crates input) (moves input)
    where moves  = map (map read)
                 . map ((\[_,x,_,y,_,z] -> [x,y,z]) . (splitOn " "))
                 . filter (isPrefixOf "move")
          crates = S.fromList
                 . filter (not . null)
                 . map (filter isAlpha)
                 . transpose
                 . filter (isPrefixOf "[")

move op crates [x, y, z] = S.adjust' (drop x) (y-1) $ S.adjust' (taken++) (z-1) crates
    where taken = op $ take x (S.index crates (y-1))