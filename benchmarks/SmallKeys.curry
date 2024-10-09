module SmallKeys where

import           Data.Trie 
import qualified Data.Map as M
import           Data.Maybe    ( fromJust )
import           Data.List     ( sum )
import           Debug.Profile ( getTimings )

alphabet :: [Char]
alphabet = ['a'..'z']

sizeAlphabet :: Int
sizeAlphabet = 26

toKey :: Int -> String
toKey n | n < 0            = error "toKey: negative number"
        | n < sizeAlphabet = [alphabet !! n]
        | otherwise        = toKey (n `div` sizeAlphabet) ++
                             toKey (n `mod` sizeAlphabet)

keys :: [String]
keys = map toKey [0..3000]

lL :: [(String, Int)] -> String -> Int
lL   = (fromJust .) . flip Prelude.lookup 

lT :: Trie Int -> String -> Int
lT   = (fromJust .) . flip Data.Trie.lookup

lMap :: M.Map String Int -> String -> Int
lMap = (fromJust .) . flip M.lookup 

sumAllT :: Trie Int -> Int
sumAllT list = sum $ map (lT list) keys

sumAllL :: [(String, Int)] -> Int
sumAllL list = sum $ map (lL list) keys

sumAllMap :: M.Map String Int -> Int
sumAllMap list = sum $ map (lMap list) keys

--- Benchmarks construction time and random access time 
--- of lists, tries and maps.
main :: IO ()
main = do 
  let lAsList = zip keys [0..]
      lAsTrie = fromList lAsList
      lAsMap  = M.fromList lAsList

  enforceNormalForm lAsList
  profile "Trie construction" (enforceNormalForm lAsTrie)
  profile "Map  construction" (enforceNormalForm lAsMap)

  profile "List lookup      " (enforceNormalForm (sumAllL   lAsList))
  profile "Trie lookup      " (enforceNormalForm (sumAllT   lAsTrie))
  profile "Map  lookup      " (enforceNormalForm (sumAllMap lAsMap))

{- Results for n=3000 keys:

> Trie construction took   200ms
> Map  construction took  1441ms
> List lookup       took 13199ms
> Trie lookup       took   916ms
> Map  lookup       took  1327ms

-}

-------------------------------------------------------------------------------
--- Auxiliary functions

enforceNormalForm :: a -> IO ()
enforceNormalForm x = normalForm x `seq` return ()

profile :: String -> IO () -> IO ()
profile nm act = do 
  (_, rt, _, _) <- getTimings act
  putStrLn (nm ++ " took " ++ show rt ++ "ms")  