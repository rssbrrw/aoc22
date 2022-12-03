module Main (main) where

import Three

main :: IO ()
main = do
    input <- lines <$> readFile ("inputs/" ++ show dayNo ++ ".txt")
    print $ solutionOne input
    print $ solutionTwo input
