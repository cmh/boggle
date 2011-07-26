{-# LANGUAGE BangPatterns #-}
module Main
    where

import Data.List (sortBy, intersect, intersperse, nub, (\\), sort)
import qualified Data.IntMap as Map
import Control.Monad (liftM)
import System.Directory (doesFileExist)
import Data.Binary (encodeFile, decodeFile)
import LetterTree
import System.Random
import System.CPUTime
import Data.Maybe (fromJust)
import Data.Sequence ()

data Board = Board
    { size :: Int
    , board :: [Char]
    } deriving (Eq)

{-data Cell = Cell-}
    {-{ id :: Int-}
    {-, char :: Char-}
    {-, neighbours :: [Cell] }-}

data Cell = Cell
    { char :: Char
    , neighbours :: [Int] } 
    deriving (Show, Eq)

--This is probably slow due to non integer keys
type BoardNeighbours = Map.IntMap Cell

fromBoard :: Board -> BoardNeighbours
fromBoard (Board n ps) = Map.fromList $ map (makeCell ps) [0 .. (n*n-1)] where
    makeCell ps k = (k, Cell c ns) where
        c = {-# SCC "GetLetter" #-} ps !! k --Doesn't scale well to large boards
        (j, i) = {-# SCC "DivMod" #-} k `divMod` n
        ns = {-# SCC "SortNeighbours" #-} sort ns'
        ns' ={-# SCC "MakeNeighbours" #-} makeNeighbours i j
        makeNeighbours i j = sort $ map (\(i, j) -> j*n + i) $ 
                             filter (\(x, y) -> (x >= 0 && y >= 0 && x < n && y < n)) $
                             [(i+1, j), (i-1, j), (i+1, j-1), (i-1, j-1), (i, j-1), (i-1,j+1), (i, j+1), (i+1, j+1)] --Could do this sorting by hand

instance Show Board where
    show (Board n ps) = sn ++ " * " ++ sn ++ " board:" ++ hLine ++ (sb ps) ++ hLine where
        sn = show n
        hLine = '\n' : replicate (2*n+1) '-'
        sb [] = ""
        sb ps = "\n|" ++ (intersperse '|' $ take n ps) ++ '|' : sb (drop n ps)

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
                query = {-# SCC "LTlookup" #-} LetterTree.lookup letterTree word --Bytestring trie
                ns' = {-# SCC "RemoveVisited" #-} ns `diff` visited  --Better dataStrucutre 
                visited' = {-# SCC "NewVisited" #-} ins loc visited  --Could just use a vector
                moreWords = concatMap (go word visited') ns'

diff [] _ = []
diff xs [] = xs
diff !(x:xs) (y:ys) | x < y  = x : diff xs (y:ys)
                    | x == y = diff xs ys
                    | x > y  = diff (x:xs) ys

ins i [] = [i]
ins i !(x:xs) | i < x = i : x : xs
              | i > x = x : ins i xs
              | otherwise = (x : xs) --Should never be hit

b4 = Board 4 "abcdefghijklmnop"
b5 = Board 5 "fdkrpvmerlksjdmepowrnckda"

randomBoard :: Int -> IO Board
randomBoard n = do
    gen <- newStdGen
    let chars = randomRs ('a', 'z') gen :: [Char]
    let boardString = take (n*n) chars
    return (Board n boardString)

generateBoards :: Int -> Int -> IO [Board]
generateBoards size n = mapM randomBoard (replicate n size)

best_words :: Board -> LetterTree -> [String]
best_words b lt = reverse . (sortBy compLength) . nub . (filter ((> 3) . length)) $ allWords b lt where
    compLength a b = (length a) `compare` (length b)

num_words b lt = length . nub . (filter ((> 3) . length)) $ allWords b lt 


treeLocation, dictLocation :: FilePath
treeLocation = "/home/chris/.boggle/wordTree.bin"
dictLocation = "/home/chris/Desktop/WORD.LST"

createWordTreeFile :: IO ()
createWordTreeFile = do
    wordList <- readFile dictLocation
    let wordTree = (fromList . lines) wordList
    encodeFile treeLocation wordTree


wordTree :: IO LetterTree
wordTree = do
    wordTreeExists <- doesFileExist treeLocation
    case wordTreeExists of
        True -> decodeFile treeLocation
        False -> do 
            putStrLn $ "Creating wordTree in " ++ treeLocation
            createWordTreeFile 
            decodeFile treeLocation

solveBoards = do
    wt <- wordTree
    putStrLn $ "Generating boards"
    let n = 50000
    boards <- generateBoards 4 n
    let len = length boards
    putStrLn $ "Generated " ++ show len ++ " random boards"
    start <- getCPUTime
    let !s = (sum . (map (flip num_words wt))) boards
    finish <- getCPUTime
    putStrLn $ show s ++ " total words"
    let runTime = (finish - start) `div` 10^9
    putStrLn $ "Solved " ++ show n ++ " boards in " ++ show runTime ++ "ms"
    let rate = fromIntegral (n * 1000) / (fromIntegral runTime)
    putStrLn $ "Rate of " ++ show rate ++ "b/s"

print_solutions board = do
    print board
    wt <- wordTree
    let numSols = num_words board wt
    let bw = best_words board wt
    putStrLn $ show numSols ++ " solutions\n"
    putStrLn $ "Best 100 solutions\n"
    putStrLn . unlines . (take 100) $ bw

main = do
    board <- randomBoard 100
    print_solutions board

{-main = solveBoards-}
