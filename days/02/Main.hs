module Main
  ( main
  ) where

import           Data.List.Split (splitOn)

import           Lib

main = runDay partOne partTwo

partOne input = sum $ map roundScore rounds
  where
    rounds =
      map (\[x, y] -> (parseThrow x, parseThrow y)) $ map (splitOn " ") input

partTwo input = sum $ map roundScore idealGame
  where
    idealGame = zip oppThrows requiredThrows
    oppThrows = map fst rounds
    results = map snd rounds
    requiredThrows = map requiredThrow $ zip oppThrows results
    rounds =
      map (\[x, y] -> (parseThrow x, parseResult y)) $ map (splitOn " ") input

data Throw
  = Rock
  | Paper
  | Scissors
  deriving (Eq)

instance Ord Throw where
  Rock > Scissors  = True
  Paper > Rock     = True
  Scissors > Paper = True
  _ > _            = False

data Result
  = Win
  | Lose
  | Draw

roundScore :: (Throw, Throw) -> Int
roundScore (oppThrow, myThrow) = myScore + resultScore
  where
    myScore = throwScore myThrow
    resultScore =
      case roundResult (oppThrow, myThrow) of
        Win  -> 6
        Lose -> 0
        Draw -> 3

requiredThrow :: (Throw, Result) -> Throw
requiredThrow (oppThrow, result) =
  case result of
    Draw -> oppThrow
    Lose ->
      case oppThrow of
        Rock     -> Scissors
        Paper    -> Rock
        Scissors -> Paper
    Win ->
      case oppThrow of
        Rock     -> Paper
        Paper    -> Scissors
        Scissors -> Rock

roundResult :: (Throw, Throw) -> Result
roundResult (oppThrow, myThrow)
  | myThrow > oppThrow = Win
  | oppThrow > myThrow = Lose
  | otherwise = Draw

throwScore :: Throw -> Int
throwScore throw =
  case throw of
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

parseThrow :: String -> Throw
parseThrow throw
  | throw `elem` ["A", "X"] = Rock
  | throw `elem` ["B", "Y"] = Paper
  | throw `elem` ["C", "Z"] = Scissors

parseResult :: String -> Result
parseResult result
  | result == "X" = Lose
  | result == "Y" = Draw
  | result == "Z" = Win
