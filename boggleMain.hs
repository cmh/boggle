{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Main
    where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.CPUTime
import System.Directory (doesFileExist, getHomeDirectory)
import System.Exit
import System.FilePath (joinPath)

import Data.Binary (encodeFile, decodeFile)

import Control.Monad (when)

import Board
import Trie

treeLocation :: IO FilePath
treeLocation = do
    homeDir <- getHomeDirectory
    return $ joinPath[homeDir,  ".boggle", "wordTree.bin"]
dictLocation :: FilePath --Currently linux only, should include cabal data file of dict
dictLocation = "/usr/share/dict/words"

--Create a binarised version of the dictionary tree and store at cache location
createWordTreeFile :: IO ()
createWordTreeFile = do
    wordList <- readFile dictLocation
    let wordTree = (fromList . lines) wordList
    tLoc <- treeLocation
    encodeFile tLoc wordTree

--Load cached binary version
--if none exists cerate one and store it
--Can use this globally as it's static
wordTree :: IO Trie
wordTree = do
    tLoc <- treeLocation
    wordTreeExists <- doesFileExist tLoc
    case wordTreeExists of
        True -> decodeFile tLoc
        False -> do 
            putStrLn $ "Creating wordTree in " ++ tLoc
            createWordTreeFile 
            decodeFile tLoc

--Create n random boards of size * size and find all solutions
--print the performance in board/s
profileBoards :: Int -> Int -> IO ()
profileBoards n size = do
    wt <- wordTree
    boards <- generateBoards size n
    let len = length boards
    putStrLn $ "Generated " ++ show len ++ " random boards"
    start <- getCPUTime
    let !s = sum $ map (`numWords` wt) boards
    finish <- getCPUTime
    putStrLn $ show s ++ " total words" --Printing here forces evaluation of s thunk, saving a seq
    let runTime = (finish - start) `div` 10^9
    putStrLn $ "Solved " ++ show n ++ " boards in " ++ show runTime ++ "ms"
    let rate = fromIntegral (n * 1000) / fromIntegral runTime
    putStrLn $ "Rate of " ++ show rate ++ "b/s"

--TODO: Don't calculate all solutions twice
printSolutions board = do
    print board
    wt <- wordTree
    let numSols = numWords board wt
    let bw = bestWords board wt
    putStrLn $ show numSols ++ " solutions\n"
    putStrLn "Best 100 solutions\n"
    putStrLn . unlines . take 100 $ bw

data MyOptions =
    Profile {size :: Int, num :: Int} |
    OneBoard {size :: Int}
    deriving (Data, Typeable, Show, Eq)

profile :: MyOptions
profile = Profile
    {num = def &= help "Number of boards to use in profiling" &= typ "INT"
    ,size = def &= help "size of the square boards" &= typ "INT" }
    &= details [ "Example:", "boggle profile 4 20000" ]

oneBoard :: MyOptions
oneBoard = OneBoard 
    {size = def &= help "Generate a random board of size n and print best solutions" &= typ "INT" }
    &= details [ "Example:", "boggle oneboard 10" ]

myModes :: Mode (CmdArgs MyOptions)
myModes = cmdArgsMode $ modes [profile, oneBoard]
    &= verbosityArgs [explicit, name "Verbose", name "V"] [] --Allow for future verbosity annotations
    &= versionArg [explicit, name "Version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "Boggle"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Automated boggle solver"
_COPYRIGHT = "(C) Chris Holdsworth 2011"

main :: IO()
main = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
    optionHandler opts

optionHandler :: MyOptions -> IO()
optionHandler opts = do
    optChecker opts
    exec opts where
        --Could have better input sanity checking
        optChecker opts@Profile{..} = when ((==0) num) $ putStrLn "Must specify the number of boards" >> exitWith (ExitFailure 1)
        optChecker opts@OneBoard{..} = when ((==0) size) $ putStrLn "Must specify a size" >> exitWith (ExitFailure 1)
        exec opts@Profile{..} = if ((==0) size) then profileBoards num 4
                                               else profileBoards num 4
        exec opts@OneBoard{..} = do
            b <- randomBoard size
            printSolutions b
