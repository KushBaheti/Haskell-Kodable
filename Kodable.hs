module Kodable where

import System.IO  
import Data.List

import MapUtils
import Move
import Check
import Solution

-- checks whether the map loaded by user is valid
-- input : map
-- output: statement describing cause of invalidity or empty string 
isValid :: [String] -> String
isValid map 
    | numOfBall /= 1   = "There must be only one ball ('@') on the map."
    | numOfTarget /= 1 = "There must be only one target ('t') on the map."
    | numOfBonus /= 3  = "Number of bonuses must be exactly 3."
    | otherwise        = ""
        where
            numOfBall   = length $ coords map '@'
            numOfTarget = length $ coords map 't'
            numOfBonus  = length $ coords map 'b'
                
-- displays available options
-- input : none
-- output: IO
options :: IO ()
options = do putStrLn ""
             putStrLn "The available options are:"
             putStrLn "1.  load str      (Loads map stored in file named 'str'.)"
             putStrLn "2.  check         (Checks if ball can reach target.)"
             putStrLn "3.  play          (Accepts moves and tests on map.)"
             putStrLn "4.  play d1 d2 d3 (Same as play, but accepts exactly three moves which can be used as functions during traversal.)"
             putStrLn "5.  hint          (When inputting moves after play has begun, can ask for a hint from current state.)"
             putStrLn "6.  save fileName (Save the game in it's current state. fileName prefix: 'saved-'; fileName suffix: '.txt')"
             putStrLn "7.  solve         (Displays optimal solution path, if one exists.)"
             putStrLn "8.  create        (Allows user to create maps by entering rows of the map and fileName to store it in. fileName prefix: 'created-'; fileName suffix: '.txt')"
             putStrLn "9.  help          (Show available options.)"
             putStrLn "10. quit          (Quits the game.)"
             putStrLn ""

-- starts the game with a map and can also be used to loads new maps within game.
-- input : file name
-- output: IO 
load :: String -> IO ()
load s = do if (length s > 4 && ((reverse $ take 4 $ reverse s) == ".txt"))
                then do contents <- readFile s
                        let map = lines contents
                        putStrLn "Read map successfully!"
                        let isSavedFile = isPrefixOf "saved-" s
                        let valid = isValid map
                        if (isSavedFile || valid == "")
                                then do options
                                        putStrLn "Initial:"
                                        printMap map
                                        start map map
                                else do putStrLn ("Invalid map. " ++ valid)
                                        putStrLn "Quitting game. Please load a new/updated map."
                else do putStrLn "Invalid file type. Please load a '.txt' file."
                        start [] []

-- creates or overwrites the file located using fileName, and saves a map in it
-- input : map, name of file
-- output: IO 
save :: [String] -> String -> IO ()
save map fileName = do let contents = unlines map
                       if (((length fileName > 12 && take 8 fileName == "created-") || (length fileName > 10 && take 6 fileName == "saved-")) && ((reverse $ take 4 $ reverse fileName) == ".txt"))
                           then do writeFile fileName contents
                                   putStrLn "Game saved succesfully!" 
                           else do putStrLn "Invalid file name/type. File name should begin with 'saved-' and end with '.txt'"
                                   putStrLn "Enter new file name below:" 
                                   newFileName <- getLine
                                   save map newFileName 

-- allows user to input rows of a map, to be saved as a custom map created by user
-- input : map, lenght of row
-- output: IO
create :: [String] -> Int -> IO ()
create map lRow = do if (lRow == -1)
                        then do putStrLn "Enter rows of map one at a time, and finally, enter the file name."
                                row <- getLine
                                create (map ++ [row]) (length row)
                        else do row <- getLine
                                if (row == "")
                                    then do if (length map == 1)
                                                then putStrLn "The map must have at least one row!"
                                                else do if ((length (last map) > 12 && take 8 (last map) == "created-"))
                                                        then save (init map) (last map)
                                                        else do putStrLn "Invalid file name. Created map files must have suffix 'created-'."
                                                                create (init map) lRow
                                    else create (map ++ [row]) lRow

-- returns original character at the coordinates the ball is currently located on
-- input : map, subsequent map after making one move
-- output: orignal character at current ball location
getNewCell :: [String] -> [String] -> Char
getNewCell m m' = if (newCell == 'b')
                    then '-'
                    else newCell
                  where
                      (x, y) = head (ballPos m')
                      newCell = (m !! x) !! y

-- tests the moves input by the user on the map
-- input : map, list of moves input by user, original character at the coordinates the ball is currently located on
-- output: modified map, stores state after making all moves
play :: [String] -> [String] -> Char -> [String] -> IO [String]
play map [] _ _                               = return (map)
play map [move] cell funcMoves                = if (move == "hint") 
                                                    then do let [hint] = take 1 (optimalSolution map)
                                                            putStrLn ("Here is a hint for you -> " ++ hint)
                                                            putStrLn "Continue entering directions from current state:"
                                                            putStrLn ""
                                                            dirs <- getDirections map 1 [] funcMoves
                                                            play map dirs cell funcMoves
                                                    else play map (move:["none"]) cell funcMoves
play map (move:nextMove:moves) cell funcMoves =  do let map' = makeMove map move nextMove cell
                                                    if (map == map') 
                                                        then do putStr "Sorry, error: cannot move to the "
                                                                if ((length move) == 1) 
                                                                    then putStrLn ("Cond{" ++ move ++ "}{" ++ nextMove ++ "}")
                                                                    else putStrLn move
                                                                putStrLn "Your current board:"
                                                                printMap map
                                                                return (map')
                                                        else do printMap map'
                                                                let bonusCount = bonusPos map
                                                                let newBonusCount = bonusPos map'
                                                                if ((length newBonusCount) < (length bonusCount))
                                                                    then putStrLn ("Got " ++ (show (3 - length(newBonusCount))) ++ "/3 bonuses!")
                                                                    else putStrLn ""
                                                                let newCell = getNewCell map map'
                                                                if (nextMove `elem` ["p", "o", "y"])
                                                                    then if [newCell] /= nextMove
                                                                            then do case nextMove of
                                                                                        "p" -> putStrLn ("Pink conditional never found")
                                                                                        "o" -> putStrLn ("Orange conditional never found")
                                                                                        "y" -> putStrLn ("Yellow conditional never found")
                                                                                    return (map')
                                                                            else play map' moves newCell funcMoves
                                                                    else if (nextMove == "none")
                                                                            then if (newCell == 't')
                                                                                    then do putStrLn "Congratulations! You win the game!"
                                                                                            putStrLn ("You collected " ++ (show (3 - length(newBonusCount))) ++ "/3 bonuses!")
                                                                                            putStrLn "Load another map to learn some more!"
                                                                                            return (map')
                                                                                    else do putStrLn "You didn't reach the target. That's alright, Please reload map and try again, or save for now!"
                                                                                            return (map')
                                                                            else play map' (nextMove:moves) newCell funcMoves
              
-- parser for conditionals, called by parseDir
-- input : direction
-- output: list of direction after validating Conditional input 
parseCond :: String -> [String]
parseCond dir = [[dir !! 5]] ++ condMove
                where
                    extractedMove = tail (init (drop 7 dir))
                    condMove = if extractedMove `elem` ["Right", "Up", "Down", "Left"] then [extractedMove] else ["-1"]

-- parser for loops, called by parseDir
-- input : direction
-- output: list of directions after vaidating & replicating the two directions of the Loop input
parseLoop :: String -> [String]
parseLoop dir
    | itr >= 0 && itr <= 5 && valid d1 && valid d2 = concat $ replicate itr ([d1] ++ [d2])
    | otherwise = parseDir "-1"
        where
            itr        = read ([dir !! 5])
            directions = init (drop 8 dir)
            [idx]      = [y | (x, y) <- zip directions [0..], x == ',']
            d1         = take idx directions
            d2         = drop (idx + 1) directions
            valid d    = d `elem` ["Right", "Up", "Down", "Left"] || take 4 d == "Cond"

-- parser for function, called by parseDir
-- input : list of moves input by user, list of Function moves
-- output: list of moves, with Function replaced by the three Function moves
parseFunc :: [String] -> [String] -> [String]
parseFunc moves funcMoves = concatMap (\move -> if (move == "Function") then funcMoves else [move]) moves

-- parser for directions input by user
-- input : direction
-- output: list of directions
parseDir :: String -> [String]
parseDir dir
    | dir `elem` ["Right", "Up", "Down", "Left", "Function"] = [dir]
    | take 4 dir == "Cond"     = if (color `elem` ['p', 'o', 'y']) then parseCond dir else ["-1"]
    | take 4 dir == "Loop"     = concat $ map parseDir (parseLoop dir)
    | otherwise                = ["-1"]
        where
            color = dir !! 5

-- once the user starts playing, this function enables the user to input directions and stores them as a list
-- input : map, integer counter (for handling print statements), list of directions, list of three Function moves (or empty list)
-- output: list of directions input by user
getDirections :: [String] -> Int -> [String] -> [String] -> IO [String]
getDirections map num ds funcMoves = do if (num == 0)
                                            then do putStr "First Direction: "
                                            else do putStr "Next Direction: "
                                        dir <- getLine
                                        if (dir == "") 
                                            then return ds
                                            else case dir of
                                                    "hint" -> return (ds ++ ["hint"])
                                                    "Function" -> do let updatedMoves = parseFunc (ds ++ ["Function"]) funcMoves
                                                                     getDirections map (num + 1) updatedMoves funcMoves
                                                    _      -> do let pDir = parseDir dir
                                                                 if ("-1" `elem` pDir)
                                                                    then return (ds ++ pDir)
                                                                    else getDirections map (num + 1) (ds ++ pDir) funcMoves

-- validates the Function moves input by user
-- input : three directions 
-- output: list of three Function directions
getFuncMoves :: String -> String -> String -> [String]
getFuncMoves d1 d2 d3 = if (validD1 && validD2 && validD3) then concat $ map (parseDir) [d1, d2, d3] else ["-1"]
                        where
                            validD1 = d1 `elem` ["Right", "Up", "Down", "Left"] || take 4 d1 == "Cond"
                            validD2 = d2 `elem` ["Right", "Up", "Down", "Left"] || take 4 d2 == "Cond"
                            validD3 = d3 `elem` ["Right", "Up", "Down", "Left"] || take 4 d3 == "Cond"

-- the Kodable command line
-- input : original map and current map (after having played and made moves, if any)
-- output: IO       
start :: [String] -> [String] -> IO ()
start map currentMap =   do inp <- getLine
                            let inps = words inp 
                            case inps of 
                                ["check"]            -> do putStrLn "Checking if solvable, this may take a few seconds..."
                                                           if (check map)
                                                               then do putStrLn "The map is solvable (ball can reach target). Enter play to begin!"
                                                                       start map currentMap
                                                               else do putStrLn "The map is not solvable. Please try again with a new/updated map."  
                                                                       start map currentMap
                                ["load", s]          -> load s 
                                ["play"]             ->  do moves <- getDirections map 0 [] []
                                                            if (length moves <= 0 || (last moves) == "-1")
                                                                then do putStrLn "Invalid direction."
                                                                        putStrLn "Please reload map and start again."
                                                                        start map currentMap
                                                                else do putStrLn ""
                                                                        putStrLn "Test:"
                                                                        putStrLn ""
                                                                        updatedMap <- play map moves '-' []
                                                                        start map updatedMap
                                ["play", d1, d2, d3] ->  do let funcMoves = getFuncMoves d1 d2 d3
                                                            if ("-1" `elem` funcMoves)
                                                                then do putStrLn "Invalid direction."
                                                                        putStrLn "Please reload map and start again."
                                                                        start map currentMap
                                                                else do moves <- getDirections map 0 [] funcMoves
                                                                        if (length moves <= 0 || (last moves) == "-1")
                                                                            then do putStrLn "Invalid direction."
                                                                                    putStrLn "Please reload map and start again."    
                                                                                    start map currentMap
                                                                            else do putStrLn ""
                                                                                    putStrLn "Test:"
                                                                                    putStrLn ""
                                                                                    updatedMap <- play map moves '-' funcMoves
                                                                                    start map updatedMap
                                ["solve"]            ->  do putStrLn "Finding optimal solution, this may take a few seconds..."
                                                            let solution = unwords $ optimalSolution map
                                                            let compressedSolution = unwords $ compressedOptimalSolution map
                                                            putStrLn "Optimal solution which collects all reachable bonuses and has least change in directions is:"
                                                            putStrLn solution
                                                            putStrLn "Compressed optimal solution:"
                                                            putStrLn compressedSolution
                                                            start map currentMap
                                ["save", fileName]   -> do save currentMap fileName
                                                           start map currentMap
                                ["create"]           -> do create [] (-1)  
                                                           start map currentMap  
                                ["help"]             -> do options
                                                           start map currentMap
                                ["quit"]             -> putStrLn "Thank you for playing Kodable, come back soon!"
                                _                    -> do putStrLn "Invalid command. Please try again."
                                                           start map currentMap

