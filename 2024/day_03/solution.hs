module Main where

import System.Environment (getArgs)

type Input = String

type Solution = Int

parser :: String -> Input
parser = undefined

solve1 :: Input -> Solution
solve1 = error "Part 1 Not implemented"

solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

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

