module Main where

import Data.Bits ((.<<.), (.>>.), (.^.))
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

-- Part 2 is definitely optimisable. Look into using a banana and seen IntMap over this.

type Input = [Int]

type Solution = Int

parser :: String -> Input
parser = map (read @Int) . lines

banana :: Int -> M.Map [Int] Int
banana secret = foldr (uncurry M.insert) M.empty $ zip groups . drop 4 $ prices
  where
    changes = zipWith (-) (tail prices) prices
    groups = stagger 4 1 changes
    prices = map (`mod` 10) . take 2_001 . iterate calc $ secret

stagger :: Int -> Int -> [Int] -> [[Int]]
stagger _ _ [] = []
stagger m n l = filter ((m ==) . length) $ cut (\xs -> (take m xs, drop n xs)) l
  where
    cut _ [] = []
    cut f as = b : cut f as'
      where
        (b, as') = f as

calc :: Int -> Int
calc = md . xod (.<<. 11) . md . xod (.>>. 5) . md . xod (.<<. 6)
  where
    xod op n = n .^. op n
    md = (`mod` 16_777_216)

solve1 :: Input -> Solution
solve1 = sum . map (\n -> iterate calc n !! 2_000)

solve2 :: Input -> Solution
solve2 = maximum . M.unionsWith (+) . map banana

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
