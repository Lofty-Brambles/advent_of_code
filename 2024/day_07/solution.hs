module Main where

import qualified Data.Foldable as F
import System.Environment (getArgs)

type Operation = Int -> Int -> [Int]

type Input = [(Int, [Int])]

type Solution = Int

parser :: String -> Input
parser = map (formPairs . words) . lines
  where
    formPairs (x : xs) = (target x, values xs)
    target x = read (takeWhile (/= ':') x)
    values = map read :: [String] -> [Int]

solve1 :: Input -> Solution
solve1 = solveBackwards [unAdd, unMultiply]

solveBackwards :: [Operation] -> Input -> Solution
solveBackwards ops = sum . map (\x -> if search x then fst x else 0)
  where
    -- x == y ? (z ? target)
    search (target, values) = head values `elem` F.foldrM go target (tail values)
    go a b = concatMap (\f -> f a b) ops

unAdd :: Operation
unAdd x y = [y - x | y >= x]

unMultiply :: Operation
unMultiply x y = [y `div` x | y `mod` x == 0]

solve2 :: Input -> Solution
solve2 = solveBackwards [unAdd, unMultiply, unCatenate]

unCatenate :: Operation
unCatenate x y = [d | m == x]
  where
    pow = length . takeWhile (< x) $ iterate (* 10) 1
    (d, m) = y `divMod` (10 ^ pow)

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
