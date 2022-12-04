module Lib
    ( runDay
    ) where

import System.Environment (getProgName)

runDay partOne partTwo = do
    progName <- getProgName
    input <- lines <$> readFile ("days/" ++ progName ++ "/input.txt")
    print $ partOne input
    print $ partTwo input