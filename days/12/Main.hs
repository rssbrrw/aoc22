{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  ) where

import Data.Char
import Data.List
import Data.Matrix

import Lib (runDay)

main = runDay partOne partTwo

data Node =
  Node
    { pos :: (Int, Int)
    , score :: Int
    }
  deriving (Show)

instance Eq Node where
  x == y = (pos x) == (pos y)

data Direction
  = Up
  | Down
  deriving (Eq)

data Q =
  Q
    { items :: [Node]
    }

add :: Q -> Node -> Q
add q n =
  case elem n (items q) of
    True -> q
    False ->
      q {items = sortBy (\a b -> compare (score a) (score b)) (n : items q)}

pop :: Q -> Node
pop q = head $ items q

-- Hard-coded start positions 🤮
partOne input = score $ search 'E' Up (Q [(Node (21, 1) 0)]) (Q []) m
  where
    m = fromLists input

partTwo input = score $ search 'a' Down (Q [(Node (21, 159) 0)]) (Q []) m
  where
    m = fromLists input

search target direction open closed matrix = do
  let current = pop open
  let newClosed = add closed current
  let curVal = getElem (fst $ pos current) (snd $ pos current) matrix
  if | curVal == target -> current
     | otherwise ->
       do let newOpen =
                foldl add (open {items = tail $ items open}) $
                neighbours direction matrix current
          search target direction newOpen newClosed matrix

neighbours :: Direction -> Matrix Char -> Node -> [Node]
neighbours direction m n =
  foldl
    (\nodes (ax, ay) ->
       case safeGet ax ay m of
         Just char ->
           case comparison (ordVal current) (comparator char) of
             True -> (Node (ax, ay) ((score n) + 1) : nodes)
             _ -> nodes
         _ -> nodes)
    []
    [((x - 1), y), ((x + 1), y), (x, (y - 1)), (x, (y + 1))]
  where
    (x, y) = pos n
    comparison =
      if | direction == Up -> (>=)
         | otherwise -> (<=)
    comparator c =
      if | direction == Up -> ((ordVal c) - 1)
         | otherwise -> ((ordVal c) + 1)
    current = getElem x y m
    ordVal c =
      case c of
        'S' -> ord 'a'
        'E' -> ord 'z'
        _ -> ord c
