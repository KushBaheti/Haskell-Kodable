import Kodable

import System.IO  
import Data.List

opposite :: String -> String
opposite "none"  = ""
opposite "Right" = "Left"
opposite "Left"  = "Right"
opposite "Up"    = "Down"
opposite "Down"  = "Up"

canGo :: [String] -> String -> Int -> Int -> [(Int, Int)] -> Bool
canGo map dir x y visited
    | dir == "Right" = notStar ((map !! x) !! (y + 2)) && notVisited (x, (y + 2))
    | dir == "Left"  = notStar ((map !! x) !! (y - 2)) && notVisited (x, (y - 2))
    | dir == "Up"    = notStar ((map !! (x - 1)) !! y) && notVisited ((x - 1), y)
    | dir == "Down"  = notStar ((map !! (x + 1)) !! y) && notVisited ((x + 1), y)
    | otherwise      = False
        where
            notStar c    = c /= '*'
            notVisited c = not $ c `elem` visited

getPossibleMoves :: [String] -> String -> (Int, Int) -> [(Int, Int)] -> [String]
getPossibleMoves map currentDirection (x,y) visited 
    | ballAtPerimeter                  = delete currentDirection . delete (opposite currentDirection) $ concat [right, left, up, down]
    | cell `elem` ['p', 'o', 'y', '@'] = delete (opposite currentDirection) $ concat [right, left, up, down]
    | nextCell /= '*'                  = if (canGo map currentDirection x y visited) then [currentDirection] else []
    | nextCell == '*'                  = delete currentDirection . delete (opposite currentDirection) $ concat [right, left, up, down]
    | otherwise                        = []
        where 
            ballAtPerimeter = x == 0 || y == 0 || x == (lRow - 1) || y == (lCol - 1)
            cell  = (map !! x) !! y
            (newX, newY) = updatedPos (x, y) currentDirection
            nextCell = (map !! newX) !! newY
            lRow  = length $ head map
            lCol  = length map
            right = if (y + 2 < lRow) && (canGo map "Right" x y visited) then ["Right"] else []
            left  = if (y - 2 >= 0)   && (canGo map "Left"  x y visited) then ["Left"]  else []
            up    = if (x - 1 >= 0)   && (canGo map "Up"    x y visited) then ["Up"]    else []
            down  = if (x + 1 < lCol) && (canGo map "Down"  x y visited) then ["Down"]  else []

updatedPos :: (Int, Int) -> String -> (Int, Int)
updatedPos (x, y) "Right" = (x, y + 2)
updatedPos (x, y) "Left"  = (x, y - 2)
updatedPos (x, y) "Up"    = (x - 1, y)
updatedPos (x, y) "Down"  = (x + 1, y)

check :: [String] -> String -> (Int, Int) -> [(Int, Int)] -> Bool
check maze currentDirection (x, y) visited
    | cell == 't' = True
    | cell /= 't' && moves == [] = False
    | cell /= 't' && moves /= [] = any (True == ) (map (\move -> check maze move (updatedPos (x, y) move) updatedVisited) moves)
        where
            cell  = (maze !! x) !! y
            moves = getPossibleMoves maze currentDirection (x, y) visited
            updatedVisited = visited ++ (map (\move -> updatedPos (x, y) move) moves)

