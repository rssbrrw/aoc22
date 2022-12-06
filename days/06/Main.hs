module Main
  ( main
  ) where

import           Data.List.Split (divvy)
import qualified Data.Set        as S (fromList, size)

import           Lib             (runDay)

main = runDay partOne partTwo

partOne = solve 4

partTwo = solve 14

solve x =
  (+ x) . length . takeWhile ((< x) . S.size . S.fromList) . divvy x 1 . head
