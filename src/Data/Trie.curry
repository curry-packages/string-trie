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
    containsKey, delete, 

  --- * Traversing functions
    insert, lookup, 
    
  --- * List conversion functions
    fromList, toList) where

import Data.List
import Data.Maybe

import Prelude                hiding  ( lookup )
import qualified Prelude as P         ( lookup )

--- The size of a trie
type Size = Int

--- Trie data structure
data Trie a = Trie Size (InternalTrie a)
  deriving (Show, Eq)

--- Internal representation of a trie.
data InternalTrie a = InternalTrie (Maybe a) [(Char, InternalTrie a)]
  deriving (Show, Eq)

--- An empty trie.
empty :: Trie a
empty = Trie 0 empty'

--- An empty internal trie.
empty' :: InternalTrie a
empty' = InternalTrie Nothing []

--- Returns true iff the trie is empty.
null :: Trie a -> Bool
null (Trie _ t) = null' t

--- Returns true iff the internal trie is empty.
null' :: InternalTrie a -> Bool
null' t = case t of 
  InternalTrie Nothing [] -> True
  _                       -> False

--- Returns the size of the trie.
size :: Trie a -> Int
size (Trie s _) = s

--- A singleton trie.
singleton :: String -> a -> Trie a
singleton = (Trie 1 .) . singleton' 

--- A singleton internal trie.
singleton' :: String -> a -> InternalTrie a
singleton' str v = foldr (\c t -> InternalTrie Nothing [(c, t)]) (InternalTrie (Just v) []) str

--- Inserts a value into the trie.
insert :: String -> a -> Trie a -> Trie a
insert key val (Trie s it) = let (incr, t) = insert' key val it
                             in Trie (if incr then s+1 else s) t 
 where 
  -- Inserts a value into the internal trie and 
  -- returns whether the size has increased.
  insert' :: String -> a -> InternalTrie a -> (Bool, InternalTrie a)
  insert' []     v (InternalTrie old ts) = case old of
    Nothing -> (True,  InternalTrie (Just v) ts)
    Just _  -> (False, InternalTrie (Just v) ts)
  insert' (c:cs) v (InternalTrie v' ts) = case P.lookup c ts of
    Nothing -> let t' = singleton' cs v 
               in (True, InternalTrie v' ((c, t') : ts))
    Just t  -> let (incr, t') = insert' cs v t
               in (incr, InternalTrie v' ((c, t') : (filter (\(c', _) -> c' /= c) ts)))

--- Removes a key from the trie. 
--- If the key is not in the trie, the trie is returned unchanged.
delete :: String -> Trie a -> Trie a
delete key (Trie s it) = case (delete' key it) of 
  Nothing -> Trie s     it
  Just t  -> Trie (s-1) t 
 where
  -- Tries to delete a key from the internal trie. If the key is not in the trie,
  -- Nothing is returned. This feedback is used to determine whether the size of the trie
  -- has changed.
  delete' :: String -> InternalTrie a -> Maybe (InternalTrie a)
  delete' k (InternalTrie v ts) = do
    r <- case k of
      []     -> case v of 
        Nothing -> Nothing
        Just _  -> Just $ InternalTrie Nothing ts
      (c:cs) -> case P.lookup c ts of
        Nothing -> Nothing
        Just t  -> do 
          t' <- delete' cs t
          return $ InternalTrie v (if null' t then filter (\(c', _) -> c' /= c) ts
                                                     else (c, t') : (filter (\(c', _) -> c' /= c) ts))
    return $ sanitize r

  sanitize :: InternalTrie a -> InternalTrie a
  sanitize (InternalTrie v ts) = InternalTrie v (filter (not . null' . snd) ts)

--- Looks up a value in the trie.
lookup :: String -> Trie a -> Maybe a
lookup key (Trie _ it) = lookup' key it
 where 
  lookup' []     (InternalTrie v _)  = v
  lookup' (c:cs) (InternalTrie _ ts) = case P.lookup c ts of
    Nothing -> Nothing
    Just t  -> lookup' cs t

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
toList (Trie _ ts) = toList' ts
 where 
  toList' (InternalTrie v ts) = case v of
    Nothing -> concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList' t)) ts
    Just z  -> ("", z) :
              concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList' t)) ts

instance Functor Trie where
  fmap f (Trie s t) = Trie s (fmap' t)
   where
    fmap' (InternalTrie v ts) = InternalTrie (fmap f v) (map (second fmap') ts)

-------------------------------------------------------------------------------
--- Auxiliary functions

second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)