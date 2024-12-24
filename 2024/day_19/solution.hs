module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as F
import System.Environment (getArgs)

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
    advance Nothing = insertion (TrieNode M.empty)
    advance (Just t') = insertion t'
    insertion p = Just $ addToTrie xs p

countM :: TrieNode -> String -> Int
countM t towel = memo M.! towel
  where
    memo = M.fromList [(xes, count xes) | xes <- L.tails towel]
    count subTowel
      | null subTowel = 1
      | otherwise = sum [memo M.! drop i subTowel | i <- nextIndices t subTowel]

nextIndices :: TrieNode -> String -> [Int]
nextIndices = parse 0
  where
    ends (TrieNode t) = '.' `M.member` t
    parse n t [] = [n | ends t]
    parse n t@(TrieNode b) (x : xs) =
      [n | ends t] ++ case M.lookup x b of
        Nothing -> []
        Just t' -> parse (n + 1) t' xs

solve1 :: Input -> Solution
solve1 (trie, towels) = length $ filter ((> 0) . countM trie) towels

solve2 :: Input -> Solution
solve2 (trie, towels) = sum $ map (countM trie) towels

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
