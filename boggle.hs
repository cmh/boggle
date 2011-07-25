module Main
    where

import Data.List (sortBy, intersect, intersperse, nub)
import Control.Monad (liftM)
import System.Directory (doesFileExist)
import Data.Binary (encodeFile, decodeFile)
import LetterTree
import System.Random

data Board = Board
    { size :: Int
    , board :: [Char]
    } deriving (Eq)

instance Show Board where
    show (Board n ps) = sn ++ " * " ++ sn ++ " board:" ++ hLine ++ (sb ps) ++ hLine where
        sn = show n
        hLine = '\n' : replicate (2*n+1) '-'
        sb [] = ""
        sb ps = "\n|" ++ (intersperse '|' $ take n ps) ++ '|' : sb (drop n ps)

--Make better names for these things, could easily make this concurrent
allWords b@(Board n s) letterTree = concatMap (wordsAt b) [0 .. (n*n-1)] where
    wordsAt (Board n ps) loc = go "" [] (loc `mod` n) (loc `div` n) where
        go soFar visited i j | index `elem` visited               = [] --already been to this square
                             | i < 0 || j < 0 || i >= n || j >= n = [] --invalid location
                             | query == None                      = [] --not a word and no more in chain
                             | query == Prefix                    = moreWords
                             | query == FullWord                  = word : moreWords where
            index = j * n + i
            char = ps !! index
            word = soFar ++ [char]
            query = LetterTree.lookup letterTree word
            visited' = index : visited
            --TODO, for the love of god make this better
            moreWords = concat [w1, w2, w3, w4, w5, w6, w7, w8]
            w1 = go word visited' (i + 1) j
            w2 = go word visited' (i - 1) j
            w3 = go word visited' (i + 1) (j + 1) 
            w4 = go word visited' (i - 1) (j + 1) 
            w5 = go word visited' i  (j + 1) 
            w6 = go word visited' (i + 1) (j - 1) 
            w7 = go word visited' (i - 1) (j - 1) 
            w8 = go word visited' i (j - 1) 

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

createWordTreeFile :: IO ()
createWordTreeFile = do
    wordList <- readFile "/usr/share/dict/words"
    let wordTree = (fromList . lines) wordList
    encodeFile treeLocation wordTree

treeLocation :: FilePath
treeLocation = "/home/chris/.boggle/wordTree.bin"

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
    boards <- generateBoards 4 100000
    let len = length boards
    putStrLn $ "Generated boards * " ++ show len
    let s = (sum . (map (flip num_words wt))) boards
    putStrLn $ "Solved boards"
    putStrLn $ show s ++ " total words"

print_solutions board = do
    print board
    wt <- wordTree
    let numSols = num_words board wt
    let bw = best_words board wt
    putStrLn $ show numSols ++ " solutions\n"
    putStrLn $ "Best 100 solutions\n"
    putStrLn . unlines . (take 100) $ bw

main = do
	board <- randomBoard 50
	print_solutions board

{-main = solveBoards-}
