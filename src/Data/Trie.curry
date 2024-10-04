------------------------------------------------------------------------------
--- A simple trie data structure which maps strings to values.
---
--- @author Lasse Züngel
--- @version October 2024
------------------------------------------------------------------------------

module Data.Trie 
  (
  --- * Data types
    Trie(), 
    
  --- * Basic functions 
    empty, null, singleton, size,
    containsKey, remove, 

  --- * Traversing functions
    insert, lookup, 
    
  --- * List conversion functions
    fromList, toList) where

import Data.List
import Data.Maybe

--- The size of a trie
type Size = Int

--- Trie data structure
data Trie a = Trie Size (Maybe a) [(Char, Trie a)]
  deriving (Show, Eq)

--- An empty trie.
empty :: Trie a
empty = Trie 0 Nothing []

--- Returns true iff the trie is empty.
null :: Trie a -> Bool
null x = case x of 
  Trie _ Nothing [] -> True
  _                 -> False

--- Returns the size of the trie.
size :: Trie a -> Int
size (Trie s _ _) = s

--- A singleton trie.
singleton :: String -> a -> Trie a
singleton str v = insert str v empty

--- Inserts a value into the trie.
insert :: String -> a -> Trie a -> Trie a
insert []     v (Trie s old ts) = case old of
  Nothing -> Trie (s+1) (Just v) ts
  Just _  -> Trie s     (Just v) ts
insert (c:cs) v (Trie s v' ts) = case Prelude.lookup c ts of
  Nothing -> Trie (s+1) v' ((c, insert cs v empty) : ts)
  Just t  -> Trie (s+1) v' ((c, insert cs v t) : (filter (\(c', _) -> c' /= c) ts))

--- Removes a key from the trie. 
--- If the key is not in the trie, the trie is returned unchanged.
remove :: String -> Trie a -> Trie a
remove key (Trie s v ts) = sanitize $ case key of
  []     -> Trie (s-1) Nothing ts
  (c:cs) -> case Prelude.lookup c ts of
    Nothing -> Trie s v ts
    Just t  -> Trie (s-1) v (if null t then filter (\(c', _) -> c' /= c) ts
                                       else (c, remove cs t) : (filter (\(c', _) -> c' /= c) ts))
 where
  sanitize (Trie s' v' ts') = case (s', v', ts') of
    (0, _, _) -> empty
    _         -> Trie s' v' ts'

--- Looks up a value in the trie.
lookup :: String -> Trie a -> Maybe a
lookup []     (Trie _ v _)  = v
lookup (c:cs) (Trie _ _ ts) = case Prelude.lookup c ts of
  Nothing -> Nothing
  Just t  -> lookup cs t

--- Checks whether a key is in the trie.
containsKey :: Trie a -> String -> Bool
containsKey = (isJust .) . flip lookup

--- Converts a list of key-value pairs into a trie.
fromList :: [(String, a)] -> Trie a
fromList = foldr (uncurry insert) empty

--- Converts a trie into a list of key-value pairs.
--- 
--- Note that no guarantees are made about the order of the elements
--- in the list.
toList :: Trie a -> [(String, a)]
toList (Trie _ v ts) = case v of
  Nothing -> concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList t)) ts
  Just z  -> ("", z) :
             concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList t)) ts

instance Functor Trie where
  fmap f (Trie s v ts) = Trie s (fmap f v) (map (second (fmap f)) ts) 

-------------------------------------------------------------------------------
--- Auxiliary functions

second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)

{-
-------- Benchmarks

alphabet :: [Char]
alphabet = ['a'..'z']

sizeAlphabet :: Int
sizeAlphabet = 26

toKey :: Int -> String
toKey n | n < 0            = error "toKey: negative number"
        | n < sizeAlphabet = [alphabet !! n]
        | otherwise        = toKey (n `div` sizeAlphabet) ++
                             toKey (n `mod` sizeAlphabet)

-- Tests:
keys = map toKey [0..1000]

input = zip keys [0..]

myList = fromList input

myMap = Data.Map.fromList input

lT :: Trie Int -> String -> Int
lT list k = fromJust $ Data.Trie.lookup k list

lP :: [(String, Int)] -> String -> Int
lP list k = fromJust $ Prelude.lookup k list

lMap :: Data.Map.Map String Int -> String -> Int
lMap list k = fromJust $ Data.Map.lookup k list

-- 487ms
sumAllT :: Trie Int -> Int
sumAllT list = sum $ map (lT list) keys

-- 1700ms
sumAllP :: [(String, Int)] -> Int
sumAllP list = sum $ map (lP list) keys

-- 930ms
sumAllMap :: Data.Map.Map String Int -> Int
sumAllMap list = sum $ map (lMap list) keys

tt = sumAllT myList  --  150ms (kics2, n=10000) -  520ms (pakcs, n=1000)
tp = sumAllP input   -- 3150ms (kics2         ) - 1750ms (pakcs        )
tm = sumAllMap myMap --  280ms (kics2         ) -  930ms (pakcs        )
-}
