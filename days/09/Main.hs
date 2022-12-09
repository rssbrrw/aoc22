module Main
  ( main
  ) where

import           Data.List (intercalate, last, scanl)
import qualified Data.Set  as S (fromList, size)

import           Lib       (runDay)

main = runDay partOne partTwo

partOne = S.size . S.fromList . map last
        . scanl moveRope (replicate 2 (0, 0)) . incrementalSteps

partTwo = S.size . S.fromList . map last
        . scanl moveRope (replicate 10 (0, 0)) . incrementalSteps

moveRope (first:rest) step = scanl follow (move first step) rest
  where
    -- | Work out the next co-ordinate for the front of the rope
    move (x, y) step =
      case step of
        'U' -> (x, y + 1)
        'D' -> (x, y - 1)
        'R' -> (x + 1, y)
        'L' -> (x - 1, y)
    
    follow (hx, hy) (tx, ty)
      | all (<2) [abs dx, abs dy] = (tx, ty) -- Don't move if we're within one square
      | otherwise = (tx + step dx, ty + step dy)
      where
        (dx, dy) = (hx - tx, hy - ty)
        step 0 = 0 
        step i = i `div` abs i 
        -- ^ Normalise any difference to +1/-1 as we can only move 1 square per turn

-- | Generate a string of individual steps from the input e.g.
--   L 2
--   R 3
--   Gives "LLDDD"
incrementalSteps =
  intercalate "" . map (\[a, b] -> replicate (read b) $ head a) . map words