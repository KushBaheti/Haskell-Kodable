module MapUtils 
    ( printMap
    , coords
    , ballPos
    , bonusPos
    ) where

import Data.List

printMap :: [String] -> IO ()
printMap []    = return ()
printMap (m:ms) = do putStrLn m
                     printMap ms

coords :: [String] -> Char -> [(Int, Int)]
coords m c = [(x, y) | (x, line) <- zip [0..] m, y <- elemIndices c line]
 
ballPos :: [String] -> [(Int, Int)]
ballPos m = coords m '@'

bonusPos :: [String] -> [(Int, Int)]
bonusPos m = coords m 'b'