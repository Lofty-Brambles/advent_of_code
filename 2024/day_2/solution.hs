module Main where

import System.Environment (getArgs)

type Input = [[Int]]

type Solution = Int

parser :: String -> Input
parser = map (map read . words) . lines

solve1 :: Input -> Solution
solve1 x = length $ filter safe x

safe :: [Int] -> Bool
safe x = valid $ diffs $ pairs x
  where
    pairs x = zip (tail x) x
    diffs = map (uncurry (-))
    valid x = all (\d -> d >= 1 && d <= 3) x || all (\d -> d >= -3 && d <= -1) x

solve2 :: Input -> Solution
solve2 x = length $ filter (any safe . dampened) x

dampened :: [Int] -> [[Int]]
dampened [] = []
dampened [x] = [[x], []]
dampened (x : xs) = xs : map (x :) (dampened xs)

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
