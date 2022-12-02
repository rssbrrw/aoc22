module Main (main) where

import Two

main :: IO ()
main = do
    input <- lines <$> readFile ("inputs/" ++ show dayNo ++ ".txt")
    print $ solutionOne input
    print $ solutionTwo input
