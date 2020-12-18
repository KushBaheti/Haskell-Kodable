module Kodable where

import System.IO  
import Data.List

import MapUtils
import Check
import Solution
import Hint

isValid :: [String] -> String
isValid map 
    | numOfBonus /= 3  = "Number of bonuses must be exactly 3."
    | numOfBall /= 1   = "There must be only one ball ('@') on the map."
    | numOfTarget /= 1 = "There must be only one target ('t') on the map."
    | otherwise        = ""
        where
            numOfBonus  = length $ coords map 'b'
            numOfBall   = length $ coords map '@'
            numOfTarget = length $ coords map 't'

load :: String -> IO ()
load s = do contents <- readFile s
            let map = lines contents
            putStrLn "Read map successfully!"
            let valid = isValid map
            if (valid == "")
                then do putStrLn "Initial:"
                        printMap map
                        start map
                else putStrLn ("Invalid map. " ++ valid)

stepRight :: String -> Int -> Char -> String
stepRight r y c = take y r ++ [c] ++ " " ++ ['@'] ++ drop (y + 3) r

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

stepLeft :: String -> Int -> Char -> String
stepLeft r y c = take (y - 2) r ++ ['@'] ++ " " ++ [c] ++ drop (y + 1) r

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
                                                    if ((length move) == 1) 
                                                        then putStrLn ("Cond{" ++ move ++ "}{" ++ nextMove ++ "}")
                                                        else putStrLn move
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
                                                                        then putStrLn "Congratulations! You win the game!"
                                                                        else putStrLn "You didn't reach the target. That's alright, try again!"
                                                                else play map' (nextMove:moves) newCell
              
parseCond :: String -> [String]
parseCond dir = [[dir !! 5]] ++ condMove
                where
                    extractedMove = tail (init (drop 7 dir))
                    condMove = if extractedMove `elem` ["Right", "Up", "Down", "Left"] then [extractedMove] else ["-1"]

parseLoop :: String -> [String]
parseLoop dir
    | itr >= 0 && itr <= 5 && valid d1 && valid d2  = concat $ replicate itr ([d1] ++ [d2])
    | otherwise             = parseDir "-1"
        where
            itr        = read ([dir !! 5])
            directions = init (drop 8 dir)
            [idx]      = [y | (x, y) <- zip directions [0..], x == ',']
            d1         = take idx directions
            d2         = drop (idx + 1) directions
            valid d    = d `elem` ["Right", "Up", "Down", "Left"] || take 4 d1 == "Cond"

parseDir :: String -> [String]
parseDir dir
    | dir `elem` ["Right", "Up", "Down", "Left", "Function"] = [dir]
    | take 4 dir == "Cond"     = if (color `elem` ['p', 'o', 'y']) then parseCond dir else ["-1"]
    | take 4 dir == "Loop"     = concat $ map parseDir (parseLoop dir)
    | otherwise                = ["-1"]
        where
            color = dir !! 5

getDirections :: [String] -> Int -> [String] -> IO [String]
getDirections map num ds = do if (num == 0)
                                then do putStr "First Direction: "
                                else do putStr "Next Direction: "
                              dir <- getLine
                              if (dir == "") 
                                then return ds
                                else do if (dir == "hint")
                                            then do let [hint] = getHint map ds
                                                    putStrLn ""
                                                    if (hint == "-1")
                                                        then putStrLn "Initial directions are incorrect. Please quit and try again!"
                                                        else do putStrLn ("Here is a hint for you -> " ++ hint)
                                                                putStrLn "Continue entering directions below:"
                                                                putStrLn ""
                                                    getDirections map (num + 1) ds
                                            else do let pDir = parseDir dir
                                                    if ("-1" `elem` pDir)
                                                        then return (ds ++ pDir)
                                                        else getDirections map (num + 1) (ds ++ pDir)

getFuncMoves :: String -> String -> String -> [String]
getFuncMoves d1 d2 d3 = if (validD1 && validD2 && validD3) then concat $ map (parseDir) [d1, d2, d3] else ["-1"]
                        where
                            validD1 = d1 `elem` ["Right", "Up", "Down", "Left"] || take 4 d1 == "Cond"
                            validD2 = d2 `elem` ["Right", "Up", "Down", "Left"] || take 4 d2 == "Cond"
                            validD3 = d3 `elem` ["Right", "Up", "Down", "Left"] || take 4 d3 == "Cond"

insertFuncMoves :: [String] -> [String] -> [String]
insertFuncMoves moves funcMoves = concatMap (\move -> if (move == "Function") then funcMoves else [move]) moves

start :: [String] -> IO ()
start map = do inp <- getLine
               let inps = words inp
               case inps of 
                   ["check"]            -> if (check map)
                                            then do putStrLn "The map is solvable! Enter play to begin."
                                                    start map
                                            else do putStrLn "The map is not solvable. Please try again with a new/updated map."  
                                                    start map
                   ["load", s]          -> load s 
                   ["play"]             -> do moves <- getDirections map 0 []
                                              if (length moves <= 0 || (last moves) == "-1")
                                                  then putStrLn "Invalid direction."
                                                  else do putStrLn "Test:"
                                                          putStrLn ""
                                                          play map moves '-'
                   ["play", d1, d2, d3] -> do let funcMoves = getFuncMoves d1 d2 d3
                                              if ("-1" `elem` funcMoves)
                                                  then putStrLn "Invalid direction."
                                                  else do moves <- getDirections map 0 []
                                                          if (length moves <= 0 || (last moves) == "-1")
                                                              then putStrLn "Invalid direction."
                                                              else do let updatedMoves = insertFuncMoves moves funcMoves
                                                                      putStrLn ""
                                                                      putStrLn "Test:"
                                                                      putStrLn ""
                                                                      play map updatedMoves '-'
                   ["solve"]            -> do let solution = unwords $ optimalSolution map
                                              putStrLn "Optimal solution which collects all bonuses and has least change in directions is:"
                                              putStrLn solution
                                              start map
                   ["quit"]             -> putStrLn "Thank you for playing Kodable, come back soon!"
                   _                    -> do putStrLn "Invalid command. Please try again."
                                              start map

