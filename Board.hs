{-# LANGUAGE BangPatterns #-}
module Board
    where

import Data.List (sortBy, intersect, intersperse, nub, (\\), sort)
import qualified Data.IntMap as Map
import Control.Monad (liftM)
import System.Random
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import Trie

data Cell = Cell
    { char :: Char
    , neighbours :: [Int] } 
    deriving (Show, Eq)

data Board = Board
    { size :: Int
    , board :: String
    } deriving (Eq)

instance Show Board where
    show (Board n ps) = sn ++ " * " ++ sn ++ " board:" ++ hLine ++ sb ps ++ hLine where
        sn = show n
        hLine = '\n' : replicate (2*n+1) '-'
        sb [] = ""
        sb ps = "\n|" ++ intersperse '|' (take n ps) ++ '|' : sb (drop n ps)

--BoardNeighbours is an additional datastructure which
--caches from frequently needed relational information about the board
--i.e. all valid neighbours of a given cell
--This is could be improved by using integer keys
type BoardNeighbours = Map.IntMap Cell

fromBoard :: Board -> BoardNeighbours
fromBoard (Board n ps) = Map.fromList $ map (makeCell ps) [0 .. (n*n-1)] where
    makeCell ps k = (k, Cell c ns) where
        c = {-# SCC "GetLetter" #-} ps !! k --Doesn't scale well to large boards (c.f. Data.Vector)
        (j, i) = {-# SCC "DivMod" #-} k `divMod` n
        ns = {-# SCC "SortNeighbours" #-} sort ns' --Sorted to allow efficiient diffs (see diff, ins below) (could encode in a type class)
        ns' ={-# SCC "MakeNeighbours" #-} makeNeighbours i j
        makeNeighbours i j = sort $ map (\(i, j) -> j*n + i) $ 
                             filter (\(x, y) -> (x >= 0 && y >= 0 && x < n && y < n))
                             [(i+1, j), (i-1, j), (i+1, j-1), (i-1, j-1), (i, j-1), (i-1,j+1), (i, j+1), (i+1, j+1)] --Could do this sorting by hand for a microoptimisation

--Make better names for these things, could easily make this concurrent
allWords b letterTree = allWords' b boardAnnotations where 
    boardAnnotations = {-# SCC "BoardAnnotations" #-} (fromBoard b)
    allWords' b@(Board n ps) bn = concatMap (wordsAt b) [0 .. (n * n - 1)] where
        wordsAt (Board n ps) loc = go "" [] loc where
            go !soFar visited loc | query == None     = [] --not a word and no more in chain
                                  | query == Prefix   = moreWords
                                  | query == FullWord = word : moreWords where
                cell@(Cell c ns) = {-# SCC "CellLookup" #-} fromJust $ Map.lookup loc bn --Vector instead of map
                word = {-# SCC "WordAppend" #-} soFar ++ [c] --Use a bytestring
                query = {-# SCC "LTlookup" #-} Trie.lookup letterTree word --Bytestring trie
                ns' = {-# SCC "RemoveVisited" #-} ns `diff` visited  --Better dataStrucutre 
                visited' = {-# SCC "NewVisited" #-} ins loc visited  --Could just use a vector
                moreWords = concatMap (go word visited') ns'

--Functions to facilitate efficient list difference over (//)
diff [] _ = []
diff xs [] = xs
diff !(x:xs) (y:ys) | x < y  = x : diff xs (y:ys)
                    | x == y = diff xs ys
                    | x > y  = diff (x:xs) ys

ins i [] = [i]
ins i !(x:xs) | i < x = i : x : xs
              | i > x = x : ins i xs
              | otherwise = x : xs --Should never be hit, could error in debug


--Generation of random boards with each letter uniformly distributed over [a,z]
--Should change at some point to reflect real boggle construction
randomBoard :: Int -> IO Board
randomBoard n = do
    gen <- newStdGen
    let chars = randomRs ('a', 'z') gen :: String
    let boardString = take (n*n) chars
    return (Board n boardString)

generateBoards :: Int -> Int -> IO [Board]
generateBoards size n = mapM randomBoard (replicate n size)

--Utility functions to extract num of solution and best solutions
bestWords :: Board -> Trie -> [String]
bestWords b lt = reverse . sortBy compLength . nub . filter ((> 3) . length) $ allWords b lt where
    compLength = comparing length

--numWords :: Board -> Trie -> Int
numWords b lt = length . nub . filter ((> 3) . length) $ allWords b lt 

