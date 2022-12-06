module Main (main) where

import Data.List.Split (divvy)
import qualified Data.Set as S (fromList, size)

import Lib (runDay)

main = runDay partOne partTwo

partOne (s:_) = firstUnique 4 s
partTwo (s:_) = firstUnique 14 s

firstUnique x = (+x) . length
              . takeWhile ((<x) . S.size . S.fromList)
              . divvy x 1