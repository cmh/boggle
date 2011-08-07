module LetterTree
    where

--A restricted trie, where ever branch node can only take one char
--larger space requirements, but easier conceptually, and may have
--better e.g. cache performance and low level optimisations

--TODO - make this an instance of relevant type classes
--Functor, Folable, TRaversable etc
--Provide a generic trie, and an efficent unpacked bytestring spcialisation

import Prelude hiding (LT, lookup)
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Data.Binary

--Differentiate between intermediate node which themselves represent words
--and those which do not
data Node = Word | NonWord
    deriving (Eq, Show)

data LetterTree = LT
    { nodeType :: Node
    , children :: Map.Map Char LetterTree}
    deriving Eq

instance Binary Node where
    put Word    = put (0 :: Word8)
    put NonWord = put (1 :: Word8)

    get = do 
        t <- get :: Get Word8
        case t of
            0 -> return Word
            1 -> return NonWord

instance Binary LetterTree where
    put (LT nt children) = do
        put nt
        put children

    get = liftM2 LT get get

topNode :: LetterTree --The start of any new tree
topNode = LT NonWord Map.empty

insert :: LetterTree -> String -> LetterTree
insert lt "" = lt
insert (LT nt children) [c] = case Map.lookup c children of
    Just lt -> LT nt (Map.adjust (\(LT nt children') -> (LT Word children')) c children)
    Nothing -> LT nt (Map.insert c (LT Word Map.empty) children)
insert (LT nt children) (c:cs) = case Map.lookup c children of
    Just lt -> LT nt (Map.adjust (const (insert lt cs)) c children)
    Nothing -> LT nt (Map.insert c (insert topNode cs) children)

instance Show LetterTree where
    show lt = showAtLevel 0 lt where
        showAtLevel n (LT nt children) = show nt ++ "\n" ++ concatMap ((concat $ replicate n "--") ++) [k : ' ' : showAtLevel (n+1) a | (k, a) <- Map.assocs children]


data Query = FullWord | Prefix | None
    deriving (Eq, Show)

lookup :: LetterTree -> String -> Query
lookup (LT nt children) [] = case nt of
    Word -> FullWord
    NonWord -> Prefix 
    --case Map.null children of
    --   True -> error "Impossible"
    --   False -> Prefix
lookup (LT nt children) (c:cs) = case Map.lookup c children of
    Nothing -> None
    Just a -> lookup a cs

fromList :: [String] -> LetterTree
fromList [] = topNode
fromList (s:ss) = insert (fromList ss) s

--Basic testData
_testTree = insert (insert (insert topNode "ape") "apple") "bee"