module MapUtils 
    ( printMap
    , coords
    , ballPos
    , bonusPos
    ) where

import Data.List

-- prints map loaded in memory to the screen in user friendly view
-- input : map
-- output: IO
printMap :: [String] -> IO ()
printMap []    = return ()
printMap (m:ms) = do putStrLn m
                     printMap ms

-- returns list of all coordinates of given character in given map as tuples of form (x, y)
-- input : map, character to find coordinates of
-- output: list of coordinates as tuples of the form (x, y)
coords :: [String] -> Char -> [(Int, Int)]
coords m c = [(x, y) | (x, line) <- zip [0..] m, y <- elemIndices c line]
 
-- returns ball position on map
-- input : map
-- output: list of ball coordinates as tuples of the form (x, y)
ballPos :: [String] -> [(Int, Int)]
ballPos m = coords m '@'

-- returns bonus positions(s) on map
-- input : map
-- output: list of bonus coordinates as tuples of the form (x, y)
bonusPos :: [String] -> [(Int, Int)]
bonusPos m = coords m 'b'