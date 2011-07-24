module LetterTree
	where

--A restricted trie, where ever branch node can only take one char
--larger space requirements, but easier conceptually

import Prelude hiding (LT, lookup)
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
insert lt "" = lt
insert (LT nt children) [c] = case Map.lookup c children of
	Just lt -> (LT nt (Map.adjust (\(LT nt children') -> (LT Word children')) c children))
	Nothing -> (LT nt (Map.insert c (LT Word Map.empty) children))
insert (LT nt children) (c:cs) = case Map.lookup c children of
	Just lt -> (LT nt (Map.adjust (const (insert lt cs)) c children))
	Nothing -> (LT nt (Map.insert c (insert topNode cs) children))

instance Show LetterTree where
	show lt = showAtLevel 0 lt where
		showAtLevel n (LT nt children) = show nt ++ "\n" ++ concatMap ((concat $ replicate n "--") ++) [k : " " ++ showAtLevel (n+1) a | (k, a) <- Map.assocs children]

data Query = FullWord | Prefix | None
	deriving (Eq, Show)

lookup :: LetterTree -> String -> Query
lookup (LT nt children) [] = case nt of
	Word -> FullWord
	NonWord -> case Map.null children of
		True -> error "Whaaaattt?!" --Get rid of this
		False -> Prefix
lookup (LT nt children) (c:cs) = case Map.lookup c children of
	Nothing -> None
	Just a -> lookup a cs

fromList :: [String] -> LetterTree
fromList [] = topNode
fromList (s:ss) = insert (fromList ss) s

testTree = insert (insert (insert topNode "ape") "apple") "bee"
testTree2 = insert topNode "abcd"
testTree3 = insert testTree2 "aefg"