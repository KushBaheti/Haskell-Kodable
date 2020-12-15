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
            start map

coords :: [String] -> Char -> [(Int, Int)]
coords m c = [(x, y) | (x, line) <- zip [0..] m, y <- elemIndices c line]
 
ballPos :: [String] -> [(Int, Int)]
ballPos m = coords m '@'

bonusPos :: [String] -> [(Int, Int)]
bonusPos m = coords m 'b'

stepRight :: String -> Int -> Char -> String
stepRight r y c = take y r ++ [c] ++ " " ++ ['@'] ++ drop (y + 3) r

moveRight :: String -> Int -> Char -> String
moveRight r y c 
    | valid && next == '-' = moveRight (stepRight r y c) (y + 2) next
    | valid && next == 'b' = moveRight (stepRight r y c) (y + 2) '-'
    | otherwise            = r
        where
            valid = (y + 2) < (length r)
            next = r !! (y + 2)

stepLeft :: String -> Int -> Char -> String
stepLeft r y c = take (y - 2) r ++ ['@'] ++ " " ++ [c] ++ drop (y + 1) r

moveLeft :: String -> Int -> Char -> String
moveLeft r y c
    | valid && next == '-' = moveLeft (stepLeft r y c) (y - 2) next
    | valid && next == 'b' = moveLeft (stepLeft r y c) (y - 2) '-'
    | otherwise            = r
        where
            valid = (y - 2) >= 0
            next = r !! (y - 2)

moveUp :: [String] -> Int -> Int -> Char -> [String]
moveUp m x y c
    | valid && next == '-' = moveUp modifiedMap (x - 1) y next
    | valid && next == 'b' = moveUp modifiedMap (x - 1) y '-'
    | otherwise            = m
        where
            valid = x >= 1
            next  = (m !! (x - 1)) !! y
            rUp      = m !! (x - 1)
            rCurrent = m !! x
            modifyRowUp      = take y rUp ++ ['@'] ++ drop (y + 1) rUp
            modifyRowCurrent = take y rCurrent ++ [c] ++ drop (y + 1) rCurrent
            modifiedMap = take (x - 1) m ++ [modifyRowUp] ++ [modifyRowCurrent] ++ drop (x + 1) m

moveDown :: [String] -> Int -> Int -> Char -> [String]
moveDown m x y c
    | valid && next == '-' = moveDown modifiedMap (x + 1) y next
    | valid && next == 'b' = moveDown modifiedMap (x + 1) y '-'
    | otherwise            = m
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

getDirections :: Int -> [String] -> IO [String]
getDirections num ds = if (num == 0)
                            then do putStr "First Direction: "
                                    dir <- getLine
                                    getDirections (num + 1) (ds ++ [dir])
                            else do putStr "Next Direction: "
                                    dir <- getLine
                                    if (dir == "") 
                                        then return ds
                                        else getDirections (num + 1) (ds ++ [dir])

getNewCell :: [String] -> [String] -> Char
getNewCell m m' = (m !! x) !! y
               where
                   (x, y) = head (ballPos m')

play :: [String] -> [String] -> Char -> IO ()
play _ [] _          = return ()
play map (m:ms) cell = do let map' = makeMove map m cell
                          if (map == map') 
                              then do putStr "Sorry, error: cannot move to the "
                                      putStrLn m
                                      putStrLn "Your current board:"
                                      printMap map
                                    --   putStrLn "Please type 'play' to enter new directions:"
                                    --   start map
                              else do printMap map'
                                      let bonusCount = bonusPos map
                                      let newBonusCount = bonusPos map'
                                      if ((length newBonusCount) == (length bonusCount) - 1)
                                          then do putStr "Got bonus "
                                                  putStrLn (show (3 - length(newBonusCount)))
                                                  putStrLn ""
                                          else putStrLn ""
                                    --   let (x, y) = head (ballPos map')
                                    --   let newCell = (map !! x) !! y
                                      let newCell = getNewCell map map'
                                      play map' ms newCell

start :: [String] -> IO ()
start map = do inp <- getLine
               if (inp == "play") 
                    then do moves <- getDirections 0 []
                            play map moves '-'
                    else do putStrLn "Invalid command."
                            start map

-- load
-- check
-- solve
-- quit
-- play
--     directions
--     test
                 
