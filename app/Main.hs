module Main (main) where


import One

main :: IO ()
main = do
    input <- lines <$> readFile ("input" ++ show dayNo ++ ".txt")
    print $ solutionOne input
    print $ solutionTwo input
