module Move ( makeMove ) where

import MapUtils

-- called by moveRight, makes the ball take one step to the right
-- input : row of map where ball is located, index where ball is located, character on which ball is located
-- output: modified row after taking one step to the right 
stepRight :: String -> Int -> Char -> String
stepRight r y c = take y r ++ [c] ++ " " ++ ['@'] ++ drop (y + 3) r

-- called by makeMove, moves the ball right
-- input : map, ball coordinates, character on which ball is located, next move
-- output: new map after having moved right
moveRight :: String -> Int -> Char -> String -> String
moveRight r y c nextMove
    | valid && nextIsTarget                      = stepRight r y c
    | valid && nextIsColor && [next] == nextMove = stepRight r y c
    | valid && next `elem` continue              = moveRight (stepRight r y c) (y + 2) next nextMove
    | valid && next == 'b'                       = moveRight (stepRight r y c) (y + 2) '-' nextMove 
    | otherwise                                  = r
        where
            valid = (y + 2) < (length r)
            next = r !! (y + 2)
            nextIsTarget = next == 't'
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y']

-- called by moveLeft, makes the ball take one step to the left
-- input : row of map where ball is located, index where ball is located, character on which ball is located
-- output: modified row after taking one step to the left
stepLeft :: String -> Int -> Char -> String
stepLeft r y c = take (y - 2) r ++ ['@'] ++ " " ++ [c] ++ drop (y + 1) r

-- called by makeMove, moves the ball left
-- input : map, ball coordinates, character on which ball is located, next move
-- output: new map after having moved left
moveLeft :: String -> Int -> Char -> String -> String
moveLeft r y c nextMove
    | valid && nextIsTarget                      = stepLeft r y c
    | valid && nextIsColor && [next] == nextMove = stepLeft r y c
    | valid && next `elem` continue              = moveLeft (stepLeft r y c) (y - 2) next nextMove
    | valid && next == 'b'                       = moveLeft (stepLeft r y c) (y - 2) '-' nextMove
    | otherwise                                  = r
        where
            valid = (y - 2) >= 0
            next = r !! (y - 2)
            nextIsTarget = next == 't'
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y']

-- called by makeMove, moves the ball up
-- input : map, ball coordinates, character on which ball is located, next move
-- output: new map after having moved up
moveUp :: [String] -> Int -> Int -> Char -> String -> [String]
moveUp m x y c nextMove
    | valid && nextIsTarget                      = modifiedMap
    | valid && nextIsColor && [next] == nextMove = modifiedMap
    | valid && next `elem` continue              = moveUp modifiedMap (x - 1) y next nextMove
    | valid && next == 'b'                       = moveUp modifiedMap (x - 1) y '-' nextMove
    | otherwise                                  = m
        where
            valid = x >= 1
            next  = (m !! (x - 1)) !! y
            rUp      = m !! (x - 1)
            rCurrent = m !! x
            modifyRowUp      = take y rUp ++ ['@'] ++ drop (y + 1) rUp
            modifyRowCurrent = take y rCurrent ++ [c] ++ drop (y + 1) rCurrent
            modifiedMap = take (x - 1) m ++ [modifyRowUp] ++ [modifyRowCurrent] ++ drop (x + 1) m
            nextIsTarget = next == 't'
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y']

-- called by makeMove, moves the ball down
-- input : map, ball coordinates, character on which ball is located, next move
-- output: new map after having moved down
moveDown :: [String] -> Int -> Int -> Char -> String -> [String]
moveDown m x y c nextMove
    | valid && nextIsTarget                      = modifiedMap
    | valid && nextIsColor && [next] == nextMove = modifiedMap
    | valid && next `elem` continue              = moveDown modifiedMap (x + 1) y next nextMove
    | valid && next == 'b'                       = moveDown modifiedMap (x + 1) y '-' nextMove
    | otherwise                                  = m
        where
            valid = x < (length m) - 1
            next  = (m !! (x + 1)) !! y
            rCurrent = m !! x
            rDown    = m !! (x + 1)
            modifyRowCurrent = take y rCurrent ++ [c] ++ drop (y + 1) rCurrent
            modifyRowDown    = take y rDown ++ ['@'] ++ drop (y + 1) rDown 
            modifiedMap = take x m ++ [modifyRowCurrent] ++ [modifyRowDown] ++ drop (x + 2) m
            nextIsTarget = next == 't'
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y']

-- called by function play (Kodable.hs), executes one move and returns the map in its new state
-- input : map, move to make, next move to make, character on which ball is located
-- output: new map after having made the move 
makeMove :: [String] -> String -> String -> Char -> [String]
makeMove map move nextMove cell
    | move == "Right" = modifyRight
    | move == "Left"  = modifyLeft
    | move == "Up"    = moveUp map x y cell nextMove
    | move == "Down"  = moveDown map x y cell nextMove
    | otherwise = map
        where
            (x, y) = head (ballPos map)
            modifyRight = take x map ++ [(moveRight (map !! x) y cell nextMove)] ++ drop (x + 1) map
            modifyLeft  = take x map ++ [(moveLeft  (map !! x) y cell nextMove)] ++ drop (x + 1) map
            moveIsColor = move `elem` ["p", "o", "y"]