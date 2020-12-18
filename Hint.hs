module Hint ( getHint ) where

import MapUtils
import Solution

makeMove :: [String] -> String -> Int -> Int -> Int -> ([String], Int, Int, Int)
makeMove map "Right" x y bonus = goRight map x y bonus
makeMove map "Left"  x y bonus = goLeft  map x y bonus
makeMove map "Up"    x y bonus = goUp    map x y bonus
makeMove map "Down"  x y bonus = goDown  map x y bonus

getHintUtil :: [String] -> [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
getHintUtil map [move] x y bonus = makeMove map move x y bonus
getHintUtil map (move:moves) x y bonus = getHintUtil newMap moves newX newY newBonus
                               where
                                   (newMap, newX, newY, newBonus) = makeMove map move x y bonus

atToDash :: [String] -> Int -> Int -> [String]
atToDash map x y = take x map ++ [modifiedRow] ++ drop (x + 1) map
                         where
                             row = map !! x
                             modifiedRow = take y row ++ "-" ++ drop (y + 1) row

updateAtPosition :: [String] -> Int -> Int -> [String]
updateAtPosition map x y = take x map ++ [modifiedRow] ++ drop (x + 1) map
                           where
                               row = map !! x
                               modifiedRow = take y row ++ "@" ++ drop (y + 1) row

getHint :: [String] -> [String] -> [String]
getHint map moves = take 1 (optimalSolution placeAt)
                    where
                        [(x, y)] = ballPos map
                        (currentMap, currentX, currentY, _) = getHintUtil map moves x y 0
                        removeAt = atToDash currentMap x y
                        placeAt = updateAtPosition removeAt currentX currentY