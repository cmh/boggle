module LetterTree
	where

--A restricted trie, where ever branch node can only take one char
--larger space requirements, but easier conceptually

import Prelude hiding (LT)
import qualified Data.Map as Map

--Differentiate between intermediate node which themselves represent words
--and those which do not
data Node = Word | NonWord 
	deriving (Eq, Show)

data LetterTree = LT
	{ nodeType :: Node
	, children :: Map.Map Char LetterTree } 
	deriving Eq

topNode = LT NonWord (Map.empty)

insert :: LetterTree -> String -> LetterTree
insert (LT nt children) [c] = (LT nt (Map.insert c (LT Word Map.empty) children))
insert (LT nt children) (c:cs) = case Map.lookup c children of
	Just lt -> (LT nt (Map.adjust (const (insert lt cs)) c children))
	Nothing -> (LT nt (Map.insert c (insert topNode cs) children))

instance Show LetterTree where
	show (LT nt children) = show nt ++ show children

--data Query = Word | Prefix | None

testTree = insert (insert (insert topNode "ape") "apple") "bee"
testTree2 = insert topNode "abcd"
testTree3 = insert testTree2 "aefg"