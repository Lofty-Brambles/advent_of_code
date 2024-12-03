module Main where

import Data.List (sort, transpose)
import System.Environment (getArgs)

type Input = [[Int]]

type Solution = Int

parser :: String -> Input
parser = map sort . transpose . map (map read . words) . lines

solve1 :: Input -> Solution
solve1 = sum . map (\[x, y] -> abs (x - y)) . transpose

solve2 :: Input -> Solution
solve2 [_, []] = 0
solve2 [[], _] = 0
solve2 [lh : ls, rh : rs]
  | lh == rh = solve2 [ls, rh : rs] + sum (takeWhile (== lh) (rh : rs))
  | lh > rh = solve2 [lh : ls, rs]
  | rh > lh = solve2 [ls, rh : rs]

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
