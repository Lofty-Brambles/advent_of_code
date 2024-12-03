module Main where

import System.Environment (getArgs)
import Text.Regex.TDFA ((=~))

type Input = [[String]]

type Solution = Int

parser :: String -> Input
parser x = x =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|don't\\(\\)|do\\(\\)" :: Input

solve1 :: Input -> Solution
solve1 x = sum $ map calc x
  where
    calc ["do()", _, _] = 0
    calc ["don't()", _, _] = 0
    calc [_, x, y] = read x * read y

solve2 :: Input -> Solution
solve2 x = snd $ foldl calc (True, 0) x
  where
    calc (_, rest) ["do()", _, _] = (True, rest)
    calc (_, rest) ["don't()", _, _] = (False, rest)
    calc (True, rest) [_, x, y] = (True, rest + read x * read y)
    calc rest [_, _, _] = rest

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
