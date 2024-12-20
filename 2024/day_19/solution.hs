module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as F
import System.Environment (getArgs)
import Text.Regex.TDFA.IntArrTrieSet (TrieSet (next))

newtype TrieNode = TrieNode (M.Map Char TrieNode) deriving (Show)

type Input = (TrieNode, [String])

type Solution = Int

parser :: String -> Input
parser x = (foldr addToTrie (TrieNode M.empty) . extract $ available, toBeMade)
  where
    extract = filter (all C.isAlpha) . L.groupBy (\a b -> all C.isAlpha [a, b])
    (available : _ : toBeMade) = lines x

addToTrie :: String -> TrieNode -> TrieNode
addToTrie [] (TrieNode t) = TrieNode $ M.insert '.' (TrieNode M.empty) t
addToTrie (x : xs) (TrieNode t) = TrieNode $ M.alter advance x t
  where
    insertion p = Just $ addToTrie xs p
    advance Nothing = insertion (TrieNode M.empty)
    advance (Just t') = insertion t'

solve1 :: Input -> Solution
solve1 (trie, towels) = length $ filter (matchStar trie) towels

matchStar :: TrieNode -> String -> Bool
matchStar trie xs =
  any terminates (matchNFA trie xs [trie])

-- matching in a non-deterministic automaton;
-- the states are lists of tries
matchNFA :: TrieNode -> String -> [TrieNode] -> [TrieNode]
matchNFA start xs tries = go xs tries
  where
    go [] ts = ts
    go (x : xs) ts =
      let ts' = submatches x ts
       in go xs ([start | any terminates ts'] ++ ts')

submatches :: Char -> [TrieNode] -> [TrieNode]
submatches x tries =
  F.catMaybes [M.lookup x branches | TrieNode branches <- tries]

counter :: TrieNode -> String -> Int
counter t pattern = memo M.! pattern
  where
    memo = M.fromList [(x, count x) | x <- L.tails pattern]
    count subPat
      | null subPat = 1
      | otherwise = sum [memo M.! drop i subPat | i <- postPrefix t pattern]

postPrefix :: TrieNode -> String -> [Int]
postPrefix = huntPrefix 0
  where
    huntPrefix n t' [] = [n | terminates t']
    huntPrefix n t'@(TrieNode t) (x : xs) = [n | terminates t'] ++ next
      where
        next = case M.lookup x t of
          Nothing -> []
          Just nextT -> huntPrefix (n + 1) nextT xs

terminates :: TrieNode -> Bool
terminates (TrieNode t) = '.' `M.member` t

solve2 :: Input -> Solution
solve2 (trie, towels) = sum . map (counter trie) $ towels

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
