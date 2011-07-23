module Main
    where

import Data.List (sortBy, intersect)
import Control.Monad (liftM)
--import Data.SetO

data Board = Board
    { size :: Int
    , board :: [Char]
    } deriving (Eq)

instance Show Board where
    show (Board n ps) = sn ++ " * " ++ sn ++ " board:" ++ (sb ps) where
        sn = show n
        sb [] = ""
        sb ps = "\n" ++ show (take n ps) ++ sb (drop n ps)

--possibles :: Board ->
possibles (Board n ps) = go 0 0 where
    go i j | j == n && i == n = []
           | i == n           = go 0 (j+1) 
           | otherwise        = (wrds n ps i j []) : go (i+1) j

wrds n ps i j locs | index `elem` locs                  = []
                   | i < 0 || j < 0 || i >= n || j >= n = []
                   | otherwise = [c] : (concatMap (map (c :)) $ filter (not . null) [w1, w2, w3, w4, w5, w6, w7, w8]) where
    index = j * n + i
    locs' = index : locs
    c = ps !! index 
    w1 = wrds n ps (i + 1) j locs'
    w2 = wrds n ps (i - 1) j locs'
    w3 = wrds n ps (i + 1) (j + 1) locs'
    w4 = wrds n ps (i - 1) (j + 1) locs'
    w5 = wrds n ps i  (j + 1) locs'
    w6 = wrds n ps (i + 1) (j - 1) locs'
    w7 = wrds n ps (i - 1) (j - 1) locs'
    w8 = wrds n ps i (j - 1) locs'

b1 = Board 1 "a"
b2 = Board 2 "abcd"
b3 = Board 3 "abcdefghi"
b4 = Board 4 "abcdefghijklmnop"

num_pos = length . concat . possibles

best_words = reverse . (sortBy compLength) . concat . possibles where
    compLength a b = (length a) `compare` (length b)

get_words :: IO [String]
get_words = liftM lines (readFile "/usr/share/dict/words")

main = do
    print b3
    words <- get_words
    let boggles = best_words b3
    let real_words = boggles `intersect` words
    print $ take 20 $ real_words
