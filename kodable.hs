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

moveRight :: String -> Int -> Char -> String -> String
moveRight r y c nextMove
    | valid && nextIsColor && [next] == nextMove = stepRight r y c
    | valid && next `elem` continue              = moveRight (stepRight r y c) (y + 2) next nextMove
    | valid && next == 'b'                       = moveRight (stepRight r y c) (y + 2) '-' nextMove 
    | otherwise                                  = r
        where
            valid = (y + 2) < (length r)
            next = r !! (y + 2)
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y', 't']

stepLeft :: String -> Int -> Char -> String
stepLeft r y c = take (y - 2) r ++ ['@'] ++ " " ++ [c] ++ drop (y + 1) r

moveLeft :: String -> Int -> Char -> String -> String
moveLeft r y c nextMove
    | valid && nextIsColor && [next] == nextMove = stepLeft r y c
    | valid && next `elem` continue              = moveLeft (stepLeft r y c) (y - 2) next nextMove
    | valid && next == 'b'                       = moveLeft (stepLeft r y c) (y - 2) '-' nextMove
    | otherwise                                  = r
        where
            valid = (y - 2) >= 0
            next = r !! (y - 2)
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y', 't']

moveUp :: [String] -> Int -> Int -> Char -> String -> [String]
moveUp m x y c nextMove
    | valid && nextIsColor && [next] == nextMove = modifiedMap
    | valid && next`elem` continue               = moveUp modifiedMap (x - 1) y next nextMove
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
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y', 't']

moveDown :: [String] -> Int -> Int -> Char -> String -> [String]
moveDown m x y c nextMove
    | valid && nextIsColor && [next] == nextMove = modifiedMap
    | valid && next == '-'                       = moveDown modifiedMap (x + 1) y next nextMove
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
            nextIsColor = next `elem` ['p', 'o', 'y']
            continue = ['-', 'p', 'o', 'y', 't']

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

getNewCell :: [String] -> [String] -> Char
getNewCell m m' = if (newCell == 'b')
                    then '-'
                    else newCell
                  where
                      (x, y) = head (ballPos m')
                      newCell = (m !! x) !! y

none :: String
none = "none"

play :: [String] -> [String] -> Char -> IO ()
play _ [] _                         = return ()
play map [move] cell                = play map (move:[none]) cell
play map (move:nextMove:moves) cell = do let map' = makeMove map move nextMove cell
                                         if (map == map') 
                                            then do putStr "Sorry, error: cannot move to the "
                                                    putStrLn move
                                                    putStrLn "Your current board:"
                                                    printMap map
                                            else do 
                                                    printMap map'
                                                    let bonusCount = bonusPos map
                                                    let newBonusCount = bonusPos map'
                                                    if ((length newBonusCount) < (length bonusCount))
                                                        then do putStr "Got "
                                                                putStr (show (3 - length(newBonusCount)))
                                                                putStrLn "/3 bonuses!"
                                                                putStrLn ""
                                                        else putStrLn ""
                                                    let newCell = getNewCell map map'
                                                    if (nextMove `elem` ["p", "o", "y"])
                                                        then if [newCell] /= nextMove
                                                                then case nextMove of
                                                                    "p" -> putStrLn ("Pink conditional never found")
                                                                    "o" -> putStrLn ("Orange conditional never found")
                                                                    "y" -> putStrLn ("Yellow conditional never found")
                                                                else play map' moves newCell
                                                        else if (nextMove == none)
                                                                then if (newCell == 't')
                                                                        then putStrLn "YOU WON!"
                                                                        else putStrLn "YOU LOST."
                                                                else play map' (nextMove:moves) newCell
                                                    

parseCond :: String -> [String]
parseCond dir = [[dir !! 5]] ++ [tail (init (drop 7 dir))]

parseLoop :: String -> [String]
parseLoop dir = [[dir !! 5]] ++ [take idx directions] ++ [drop (idx + 1) directions]
                where
                    directions = init (drop 8 dir)
                    idx        = head [y | (x, y) <- zip directions [0..], x == ',']

parseDir :: String -> [String]
parseDir dir
    | dir `elem` ["Right", "Up", "Down", "Left", "Function"] = [dir]
    | take 4 dir == "Cond" = parseCond dir
    | take 4 dir == "Loop" = parseLoop dir

-- Loop{n}{Direction,Direction}     n = [0,5]
getDirections :: Int -> [String] -> IO [String]
getDirections num ds = if (num == 0)
                            then do putStr "First Direction: "
                                    dir <- getLine
                                    getDirections (num + 1) (ds ++ [dir])
                            else do putStr "Next Direction: "
                                    dir <- getLine
                                    if (dir == "") 
                                        then return ds
                                        else do let pDir = parseDir dir
                                                getDirections (num + 1) (ds ++ pDir)

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
                 
