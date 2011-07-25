module Trie
    where

import qualified Data.Map as Map

data Node = Word | NonWord --E.g. complete words or just intermediate stage
    deriving (Eq, Show)

data Trie = 
    { letters   :: [Char]
    , isword    :: Node
    , children  :: Map.Map Trie } deriving Eq
    
instance Show Trie where
    show t = unlines $ make_list t where
        make_list (Trie l w c) = l ++ " (" ++ show w ++ ")\n" : indented_children where
            indented_children = map ("\t" :) (make_list c)

test = Trie ("a" NonWord Map.fromList [Trie ("t"