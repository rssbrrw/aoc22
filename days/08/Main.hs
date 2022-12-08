module Main
  ( main
  ) where

import           Data.List   (any, map, maximum)
import           Data.Matrix (fromLists, getCol, getElem, getRow, ncols, nrows)
import qualified Data.Vector as V (all, drop, reverse, take, toList)

import           Lib         (runDay)

main = runDay partOne partTwo

partOne input =
  length $
  filter (== True) [isVisible m x y | x <- [1 .. ncols m], y <- [1 .. nrows m]]
  where
    m = fromLists input

partTwo input =
  maximum $ [scenicScore m x y | x <- [1 .. ncols m], y <- [1 .. nrows m]]
  where
    m = fromLists input

isVisible m x y =
  any (== True) $
  map
    (V.all (< el))
    [V.take (y - 1) row, V.drop y row, V.take (x - 1) col, V.drop x col]
  where
    el = getElem x y m
    row = getRow x m
    col = getCol y m

scenicScore m x y =
  product $
  map
    (viewDistance . V.toList)
    [ V.reverse (V.take y row)
    , V.drop (y - 1) row
    , V.reverse (V.take x col)
    , V.drop (x - 1) col
    ]
  where
    el = getElem x y m
    row = getRow x m
    col = getCol y m
    viewDistance (x:xs)
      | null xs = 0
      | x <= head xs = 1
      | otherwise = 1 + viewDistance (x : tail xs)
