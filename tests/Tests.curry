module Tests where

import Data.Trie
import Test.Prop
import Data.Maybe ( isNothing )
import Data.List  ( sort, nub, intersect )

import           Prelude hiding ( lookup, null, empty )
import qualified Prelude as P   ( null )

--- Property: an empty trie has size 0.
testEmptySize :: Prop
testEmptySize = null empty -=- True

--- Property: a singleton trie has size 1.
testSingletonSize :: Prop
testSingletonSize = size (singleton "a" 42) -=- 1

--- Property: the size of a trie is equal to the number of elements in it.
testSize :: [(String, Int)] -> Prop
testSize input = uniqueKeys input 
             ==> size (fromList input) -=- length input

--- Property: all elements in a trie can be looked up.
testKeys :: [(String, Int)] -> Prop
testKeys c = forAll keys $ \k -> trie `containsKey` k -=- True
 where
  keys = map fst c
  trie = fromList c

--- Property: trying to look up a non-existing key in a trie is unsuccessful.
testExtranousKeys :: [(String, Int)] -> Prop
testExtranousKeys input = forAll extra $ \k -> trie `containsKey` k -=- False
 where
  keys  = map fst input
  trie  = fromList input
  extra = take (length input) $ freshKeys keys

--- Property: removing one key from a non-empty trie results in a trie with one less element,
---           namely, the one associated with the removed key.
testRemoveOne :: [(String, Int)] -> Prop
testRemoveOne input = not (P.null input) 
                   ==> let nt = remove k trie 
                       in size nt == (size trie - 1) && isNothing (lookup k nt) -=- True
 where
  keys = map fst input
  trie = fromList input
  k    = head keys

--- Property: removing all keys from a trie results in an empty trie.
testRemove :: [(String, Int)] -> Prop
testRemove input = (foldr remove trie keys) -=- empty
 where
  keys = nub $ map fst input
  trie = fromList input

--- Property: removing a non-existing key from a trie does not change the trie.
testRemoveNonExisting :: [(String, Int)] -> Prop
testRemoveNonExisting input = forAll extra $ \k -> remove k trie -=- trie
 where
  keys  = nub $ map fst input
  trie  = fromList input
  extra = take (length input) $ freshKeys keys

--- Property: `toList . fromList` behaves like the identity function 
--- (ignoring potential loss of order). That is, all information is preserved.
testConversion :: [(String, Int)] -> Prop
testConversion input = uniqueKeys content 
                   ==> (sort . toList . fromList) content -=- sort content
 where
  content = input ++ [("a", 42), ("b", 43), ("ab", 44)]

--- Tests functor intsance of Data.Trie.
testFmap :: [(String, Int)] -> Prop
testFmap input = uniqueKeys input
            ==> (sort . toList . fmap (+1) . fromList) input -=- (sort . map (\(k, v) -> (k, v+1))) input

-------------------------------------------------------------------------------
--- Auxiliary functions

--- Checks whether the keys in the input list are unique.
uniqueKeys :: [(String, a)] -> Bool
uniqueKeys xs = length xs == (length . nub . map fst) xs

--- Checks whether two lists are disjoint.
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint = (P.null .) . intersect

--- Generates an infinite list of fresh keys.
freshKeys :: [String] -> [String]
freshKeys keys = filter (`notElem` keys) (map show [1..])