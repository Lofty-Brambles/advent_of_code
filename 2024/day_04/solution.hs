{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}

module Main where

import qualified Data.List as L
import qualified Data.Map as M
import System.Environment (getArgs)

type Pair = (Int, Int)

type Input = M.Map Pair Char

type Solution = Int

parser :: String -> Input
parser = M.fromList . concat . zipWith enum_row [0 ..] . lines
  where
    enum_row y = zipWith (map_to_z y) [0 ..]
    map_to_z y x ch = ((x, y), ch)

solve1 :: Input -> Solution
solve1 x = sum . map spread $ M.keys x
  where
    spread pair = length $ filter (checkXMAS x pair) diffs
    diffs = drop 1 [(x, y) | x <- shifts, y <- shifts] :: [Pair]
    shifts = [0, 1, -1]

checkXMAS :: Input -> Pair -> Pair -> Bool
checkXMAS store origin incr = (==) "XMAS" $ map (findIt store) getCoords
  where
    getCoords = map (\k -> opPairs (getIncr k) origin incr) [0, 1, 2, 3]
    opPairs f (x, y) (u, v) = (f x u, f y v)
    getIncr k i j = i + j * k

findIt :: Input -> Pair -> Char
findIt store x = if M.member x store then store M.! x else '.'

solve2 :: Input -> Solution
solve2 x = length $ filter condition $ M.keys x
  where
    condition e = (findIt x e == 'A') && matcher e
    matcher e = all (diagMatcher x e ms) [prinDiag, otherDiag]
    prinDiag = [(-1, -1), (1, 1)] :: [Pair]
    otherDiag = [(-1, 1), (1, -1)] :: [Pair]
    ms = [['M', 'S'], ['S', 'M']]

diagMatcher :: Input -> Pair -> [[Char]] -> [Pair] -> Bool
diagMatcher x base chars pairs = mapIncr pairs `elem` chars
  where
    mapIncr = map (findIt x . incrIndex base)
    incrIndex p by = (fst p + fst by, snd p + snd by)

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
