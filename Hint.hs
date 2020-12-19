module Hint ( getHint ) where

import MapUtils
import Solution

makeMove :: [String] -> String -> Int -> Int -> Int -> ([String], Int, Int, Int)
makeMove map dir x y bonus 
    | dir == "Right" = goRight map x y bonus
    | dir == "Left" = goLeft map x y bonus
    | dir == "Up" = goUp map x y bonus
    | dir == "Down" = goDown map x y bonus
    | otherwise = (map, -1, -1, bonus)

getHintUtil :: [String] -> [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
getHintUtil map [move] x y bonus = getHintUtil map (move:["none"]) x y bonus
getHintUtil map (move:moves) x y bonus
    | x == newX && y == newY = ([], -1, -1, -1)
    | newX == -1 && newY == -1 = if (isColor move) then getHintUtil map moves x y bonus else ([], -1, -1, -1)
    | moves == ["none"] = (newMap, newX, newY, newBonus)
    | otherwise = getHintUtil newMap moves newX newY newBonus
        where
            (newMap, newX, newY, newBonus) = makeMove map move x y bonus
            isColor c = [((map !! x) !! y)] == c


getHint :: [String] -> [String] -> [String]
getHint map moves
    | moves == [] = take 1 (optimalSolutionForHint map x y 0) 
    | otherwise   = if currentMap == [] then ["-1"] else take 1 (optimalSolutionForHint currentMap currentX currentY currentBonus)
        where
            [(x, y)] = ballPos map
            (currentMap, currentX, currentY, currentBonus) = getHintUtil map moves x y 0