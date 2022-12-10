module Main
  ( main
  ) where

import           Data.List.Split
import           Data.Matrix

import           Lib             (runDay)

main = runDay partOne partTwo

partOne input =
  sum $
  map
    (\x -> ((* x) $ snd $ last $ takeWhile (\(a, b) -> a < x) seq))
    [20, 60, 100, 140, 180, 220]
  where
    seq = scanl next (0, 1) input

partTwo input =
  fromList 6 1 $ chunksOf 40 $ map (pixelValue . cursorPos seq) [0 .. 239]
  where
    seq = scanl next (0, 1) input
    pixelValue (a, b)
      | elem (a `mod` 40) [b - 1, b, b + 1] = '#'
      | otherwise = ' '
    cursorPos seq x =
      (\(a, b) -> (x, b)) $ last $ takeWhile (\(a, b) -> a <= x) seq

next (cyc, reg) op =
  case words op of
    ["noop"]            -> ((cyc + 1), reg)
    ["addx", ('-':num)] -> ((cyc + 2), (reg - (read num)))
    ["addx", num]       -> ((cyc + 2), (reg + (read num)))
