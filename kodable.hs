import System.IO  
import Data.List

printMap :: [String] -> IO ()
printMap []    = return ()
printMap (m:ms) = do putStrLn m
                     printMap ms

load :: String -> IO ()
load s = do contents <- readFile s
            let map = lines contents
            putStrLn "Read map successfully!"
            putStrLn "Initial:"
            printMap map
 
ballPos :: [String] -> [(Int, Int)]
ballPos m = [(x, y) | (x, line) <- zip [0..] m, y <- elemIndices '@' line] 

stepRight :: String -> Int -> Char -> String
stepRight r y c = take y r ++ [c] ++ " " ++ ['@'] ++ drop (y + 3) r

moveRight :: String -> Int -> Char -> String
moveRight r y c = if (valid && next == '-')
                    then moveRight (stepRight r y c) (y + 2) next
                    else r
                  where
                    valid = (y + 2) < (length r)
                    next = r !! (y + 2)

stepLeft :: String -> Int -> Char -> String
stepLeft r y c = take (y - 2) r ++ ['@'] ++ " " ++ [c] ++ drop (y + 1) r

moveLeft :: String -> Int -> Char -> String
moveLeft r y c = if (valid && next == '-')
                    then moveLeft (stepLeft r y c) (y - 2) next
                    else r
                  where
                    valid = (y - 2) >= 0
                    next = r !! (y - 2)

moveUp :: [String] -> Int -> Int -> Char -> [String]
moveUp m x y c = if (valid && next == '-')
                    then moveUp modifiedMap (x - 1) y next
                    else m
                 where
                    valid = x >= 1
                    next  = (m !! (x - 1)) !! y
                    rUp      = m !! (x - 1)
                    rCurrent = m !! x
                    modifyRowUp      = take y rUp ++ ['@'] ++ drop (y + 1) rUp
                    modifyRowCurrent = take y rCurrent ++ [c] ++ drop (y + 1) rCurrent
                    modifiedMap = take (x - 1) m ++ [modifyRowUp] ++ [modifyRowCurrent] ++ drop (x + 1) m

moveDown :: [String] -> Int -> Int -> Char -> [String]
moveDown m x y c = if (valid && next == '-')
                        then moveDown modifiedMap (x + 1) y next
                        else m
                   where
                        valid = x < (length m) - 1
                        next  = (m !! (x + 1)) !! y
                        rCurrent = m !! x
                        rDown    = m !! (x + 1)
                        modifyRowCurrent = take y rCurrent ++ [c] ++ drop (y + 1) rCurrent
                        modifyRowDown    = take y rDown ++ ['@'] ++ drop (y + 1) rDown 
                        modifiedMap = take x m ++ [modifyRowCurrent] ++ [modifyRowDown] ++ drop (x + 2) m

makeMove :: [String] -> String -> Char -> [String]
makeMove map command cell
    | command == "Right" = modifyRight
    | command == "Left"  = modifyLeft
    | command == "Up"    = moveUp map x y cell
    | command == "Down"  = moveDown map x y cell
    | otherwise = map
        where
            (x, y) = head (ballPos map)
            modifyRight = take x map ++ [(moveRight (map !! x) y cell)] ++ drop (x + 1) map
            modifyLeft  = take x map ++ [(moveLeft  (map !! x) y cell)] ++ drop (x + 1) map

-- load
-- check
-- solve
-- quit
-- play
--     directions
--     test
                 
