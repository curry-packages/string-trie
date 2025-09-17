-- | Description: A simple trie data structure which maps strings to values
--   Author     : Lasse Züngel
--   Version    : September 2025

module Data.Trie
  (
  -- * Data types
    Trie(),

  -- * Basic functions
    empty, null, singleton, size,
    containsKey,

  -- * Traversing functions
    insert, lookup, update, delete,

  -- * List conversion functions
    fromList, toList) where

import qualified Data.Trie.Internal as I

import Data.List
import Data.Maybe

-- | The size of a trie.
type Size = Int

-- | Trie data structure.
data Trie a = Trie Size (I.InternalTrie a)
  deriving (Show, Eq)

-- | An empty trie.
empty :: Trie a
empty = Trie 0 I.empty

-- | Returns true iff the trie is empty.
null :: Trie a -> Bool
null (Trie _ t) = I.null t

-- | Returns the size of the trie.
size :: Trie a -> Int
size (Trie s _) = s

-- | A singleton trie.
singleton :: String -> a -> Trie a
singleton = Trie 1 .: I.singleton

-- | Inserts a value into the trie.
insert :: String -> a -> Trie a -> Trie a
insert key val = update key (const val)

-- | Updates or inserts a value in the trie.
update :: String -> (Maybe a -> a) -> Trie a -> Trie a
update key f (Trie s it) = let (incr, t) = I.update key f it
                           in Trie (if incr then s+1 else s) t

-- | Removes a key from the trie.
--   If the key is not in the trie, the trie is returned unchanged.
delete :: String -> Trie a -> Trie a
delete key (Trie s it) = case (I.delete key it) of
  Nothing -> Trie s     it
  Just t  -> Trie (s-1) t

-- | Looks up a value in the trie.
lookup :: String -> Trie a -> Maybe a
lookup key (Trie _ it) = I.lookup key it

-- | Checks whether a key is in the trie.
containsKey :: Trie a -> String -> Bool
containsKey = isJust .: flip lookup

-- | Converts a list of key-value pairs into a trie.
fromList :: [(String, a)] -> Trie a
fromList = foldr (uncurry insert) empty

-- | Converts a trie into a list of key-value pairs.
--
--   Note that no guarantees are made about the order of the elements
--   in the list.
toList :: Trie a -> [(String, a)]
toList (Trie _ ts) = I.toList ts

instance Functor Trie where
  fmap f (Trie s t) = Trie s (fmap f t)

--------------------------------------------------------------------------------
-- Helper functions

infixr 9 .:

-- | Composition operator similar to (.), but the second function has two arguments.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
