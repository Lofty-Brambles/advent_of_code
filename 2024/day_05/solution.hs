module Main where

import qualified Data.List as L
import System.Environment (getArgs)

type Input = [([Int], [Int])]

type Solution = Int

parser :: String -> Input
parser x = sorted rules updates
  where
    rules = map (splitter '|') $ fst halves
    updates = map (splitter ',') $ tail $ snd halves
    halves = break null $ lines x
    splitter a = map read . splitOn a

sorted :: [[Int]] -> [[Int]] -> Input
sorted rules = map (\ele -> (ele, L.sortBy order ele))
  where
    order a b
      | [a, b] `elem` rules = LT
      | [b, a] `elem` rules = GT
      | otherwise = EQ

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn chr = L.unfoldr sep
  where
    sep [] = Nothing
    sep l = Just . fmap (drop 1) . break (== chr) $ l

solve1 :: Input -> Solution
solve1 = sum . map value
  where
    value (left, right)
      | left == right = middle left
      | otherwise = 0

solve2 :: Input -> Solution
solve2 = sum . map value
  where
    value (left, right)
      | left /= right = middle right
      | otherwise = 0

middle :: [Int] -> Int
middle x = x !! (length x `div` 2)

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
