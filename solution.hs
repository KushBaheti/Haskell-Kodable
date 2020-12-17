import Kodable

removeBonus :: [String] -> Int -> Int -> [String]
removeBonus map x y = take x map ++ [modifiedRow] ++ drop (x + 1) map
                      where
                          row = map !! x
                          modifiedRow = take y row ++ "-" ++ drop (y + 1) row

goRight :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goRight map x y bonusCount
    | y + 2 >= ((length $ head map) - 1) || (map !! x) !! (y + 2) == '*' = (map, x, y, bonusCount) 
    | (map !! x) !! (y + 2) == 'b' = goRight (removeBonus map x y) x (y + 2) (bonusCount + 1)
    | (map !! x) !! (y + 2) `elem` ['p', 'o', 'y', 't'] = (map, x, (y + 2), bonusCount)
    | (map !! x) !! (y + 2) == '-' = goRight map x (y + 2) bonusCount

goLeft :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goLeft map x y bonusCount
    | y - 2 < 0 || (map !! x) !! (y - 2) == '*' = (map, x, y, bonusCount) 
    | (map !! x) !! (y - 2) == 'b' = goLeft (removeBonus map x y) x (y - 2) (bonusCount + 1)
    | (map !! x) !! (y - 2) `elem` ['p', 'o', 'y', 't'] = (map, x, (y - 2), bonusCount)
    | (map !! x) !! (y - 2) == '-' = goLeft map x (y - 2) bonusCount

goUp :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goUp map x y bonusCount
    | x - 1 < 0 || (map !! (x - 1)) !! y == '*' = (map, x, y, bonusCount) 
    | (map !! (x - 1)) !! y == 'b' = goUp (removeBonus map x y) (x - 1) y (bonusCount + 1)
    | (map !! (x - 1)) !! y `elem` ['p', 'o', 'y', 't'] = (map, (x - 1), y, bonusCount)
    | (map !! (x - 1)) !! y == '-' = goUp map (x - 1) y bonusCount

goDown :: [String] -> Int -> Int -> Int -> ([String], Int, Int, Int)
goDown map x y bonusCount
    | x + 1 > ((length map) - 1) || (map !! (x + 1)) !! y == '*' = (map, x, y, bonusCount) 
    | (map !! (x + 1)) !! y == 'b' = goDown (removeBonus map x y) (x + 1) y (bonusCount + 1)
    | (map !! (x + 1)) !! y `elem` ['p', 'o', 'y', 't'] = (map, (x + 1), y, bonusCount)
    | (map !! (x + 1)) !! y == '-' = goDown map (x + 1) y bonusCount

-- -- t @ - p o y b * {IoB}
-- optimalSolutionUtil :: [String] -> [String]
-- optimalSolutionUtil map x y currentDirection visited allSolutions currentSolution bonusCount
--     | cell == 't' && bonusCount == 3 = allSolutions ++ [currentSolution] 
--     | isColor cell || isAt cell = allSolutions ++ [rightPath] ++ [leftPath] ++ [upPath] ++ [downPath]