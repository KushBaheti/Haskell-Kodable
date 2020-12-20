module Solution 
    ( optimalSolution
    , optimalSolutionUtil
    , shortestSolution
    , goRight
    , goLeft
    , goUp
    , goDown
    , removeBonus
    ) where

import Data.List
import Data.Ord

import MapUtils

shortestSolution solutions = minimumBy (comparing length) solutions

removeBonus :: [String] -> Int -> Int -> [String]
removeBonus map x y = take x map ++ [modifiedRow] ++ drop (x + 1) map
                      where
                          row = map !! x
                          modifiedRow = take y row ++ "-" ++ drop (y + 1) row

goRight :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goRight map x y bonus
    | y + 2 >= (length $ head map) || (map !! x) !! (y + 2) == '*' = (map, x, y, bonus) 
    | (map !! x) !! (y + 2) == 'b' = goRight (removeBonus map x (y+2)) x (y + 2) (bonus + 1)
    | (map !! x) !! (y + 2) `elem` ['p', 'o', 'y', 't'] = (map, x, (y + 2), bonus)
    | otherwise = goRight map x (y + 2) bonus

goLeft :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goLeft map x y bonus
    | y - 2 < 0 || (map !! x) !! (y - 2) == '*' = (map, x, y, bonus) 
    | (map !! x) !! (y - 2) == 'b' = goLeft (removeBonus map x (y-2)) x (y - 2) (bonus + 1)
    | (map !! x) !! (y - 2) `elem` ['p', 'o', 'y', 't'] = (map, x, (y - 2), bonus)
    | otherwise = goLeft map x (y - 2) bonus

goUp :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goUp map x y bonus
    | x - 1 < 0 || (map !! (x - 1)) !! y == '*' = (map, x, y, bonus) 
    | (map !! (x - 1)) !! y == 'b' = goUp (removeBonus map (x-1) y) (x - 1) y (bonus + 1)
    | (map !! (x - 1)) !! y `elem` ['p', 'o', 'y', 't'] = (map, (x - 1), y, bonus)
    | otherwise = goUp map (x-1) y bonus

goDown :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goDown map x y bonus
    | x + 1 >= (length map) || (map !! (x + 1)) !! y == '*' = (map, x, y, bonus) 
    | (map !! (x + 1)) !! y == 'b' = goDown (removeBonus map (x+1) y) (x + 1) y (bonus + 1)
    | (map !! (x + 1)) !! y `elem` ['p', 'o', 'y', 't'] = (map, (x + 1), y, bonus)
    | otherwise = goDown map (x+1) y bonus

optimalSolutionUtil :: [String] -> Int -> Int -> [(Int, Int, Int)] -> [String] -> Int -> Int -> [[String]]
optimalSolutionUtil map x y visited solution bonus targetBonus
    | cell == 't' && bonus /= targetBonus = []
    | cell == 't' && bonus == targetBonus = [solution]
    | isColor cell = (rightPath (condString cell "Right")) ++ (leftPath (condString cell "Left")) ++ (upPath (condString cell "Up")) ++ (downPath (condString cell "Down"))
    | otherwise = (rightPath "Right") ++ (leftPath "Left") ++ (upPath "Up") ++ (downPath "Down")
        where
            cell = (map !! x) !! y
            isColor c = c `elem` ['p', 'o', 'y']
            condString cell s = "Cond{" ++ [cell] ++ "}{" ++ s ++ "}"
            (mapR, xR, yR, bonusR) = goRight map x y bonus
            rightPath s = if ((x == xR && y == yR) || (xR, yR, bonusR) `elem` visited) then [] else optimalSolutionUtil mapR xR yR (visited ++ [(xR, yR, bonusR)]) (solution ++ [s]) bonusR targetBonus
            (mapL, xL, yL, bonusL) = goLeft map x y bonus
            leftPath s  = if ((x == xL && y == yL) || (xL, yL, bonusL) `elem` visited) then [] else optimalSolutionUtil mapL xL yL (visited ++ [(xL, yL, bonusL)]) (solution ++ [s]) bonusL targetBonus
            (mapU, xU, yU, bonusU) = goUp map x y bonus
            upPath s    = if ((x == xU && y == yU) || (xU, yU, bonusU) `elem` visited) then [] else optimalSolutionUtil mapU xU yU (visited ++ [(xU, yU, bonusU)]) (solution ++ [s]) bonusU targetBonus
            (mapD, xD, yD, bonusD) = goDown map x y bonus
            downPath s  = if ((x == xD && y == yD) || (xD, yD, bonusD) `elem` visited) then [] else optimalSolutionUtil mapD xD yD (visited ++ [(xD, yD, bonusD)]) (solution ++ [s]) bonusD targetBonus

condense :: [String] -> [String]
condense [] = []
condense [move] = [move]
condense (move:nextMove:moves) = if (sameDirection) 
                                    then [move] ++ condense moves 
                                    else [move] ++ condense (nextMove:moves)
                                 where
                                     sameDirection = take 4 nextMove == "Cond" && 
                                                     (length nextMove) >= 11   &&
                                                     move == (init $ drop 8 nextMove)

            -- parsePath move
            --  | ((length path)>0) && (isLoop (lastToLast!!0)) && ((getLoopMoves (lastToLast!!0)) == (last ++ move))   = (take ((length path)-2) path) ++ [(increaseItr (lastToLast!!0))]
            --  | ((length path)>2) && ((last ++ move) == lastTwo) && (not (isLoop (last!!0)))  = (take ((length path)-3) path) ++ ["Loop{2}{"++(lastTwo!!0)++","++(lastTwo!!1)++"}"]
            --  | otherwise    = path ++ move

optimalSolution :: [String] -> [String]
optimalSolution maze
    | canReach3Bonus /= [] = shortestSolution $ map condense canReach3Bonus
    | canReach2Bonus /= [] = shortestSolution $ map condense canReach2Bonus
    | canReach1Bonus /= [] = shortestSolution $ map condense canReach1Bonus
    | canReach0Bonus /= [] = shortestSolution $ map condense canReach0Bonus
    | otherwise            = ["Not solvable."]
        where
            [(x, y)] = ballPos maze
            canReach3Bonus = optimalSolutionUtil maze x y [(x, y, 0)] [] 0 3
            canReach2Bonus = optimalSolutionUtil maze x y [(x, y, 0)] [] 0 2
            canReach1Bonus = optimalSolutionUtil maze x y [(x, y, 0)] [] 0 1
            canReach0Bonus = optimalSolutionUtil maze x y [(x, y, 0)] [] 0 0  

