module Main where

import Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import System.Environment (getArgs)

type Robot = [[Int]]

type Input = [Robot]

type Solution = Int

parser :: String -> Input
parser = map (arrange . extract) . lines
  where
    arrange = (\(a : b : c : d : _) -> [[a, b], [c, d]]) . map (read @Int)
    extract = filter (all numCheck) . L.groupBy (\a b -> numCheck a && numCheck b)
    numCheck x = C.isDigit x || (x == '-')

boardSize :: [Int]
boardSize = [101, 103] -- [11, 7] --

solve1 :: Input -> Solution
solve1 robots = score $ map (advance 100) robots

advance :: Int -> Robot -> Robot
advance ticks [pos, velocity] = [wrapped, velocity]
  where
    wrapped = zipWith mod unwrapped boardSize
    unwrapped = zipWith (+) pos $ map (* ticks) velocity

score :: [Robot] -> Int
score robots = product $ map getScore [q1, q2, q3, q4]
  where
    getScore condition = length $ filter condition $ map head robots
    q1 [x, y] = x > bbx && y > bby
    q2 [x, y] = x < bbx && y > bby
    q3 [x, y] = x < bbx && y < bby
    q4 [x, y] = x > bbx && y < bby
    [bbx, bby] = map (`div` 2) boardSize

solve2 :: Input -> IO ()
solve2 robots = do
  let process ticks = (ticks, score $ map (advance ticks) robots)
  let scores = map process [0 .. 10_000]
  let p@(ticks, bestScore) = L.minimumBy (O.comparing snd) scores
  putStrLn $ "Answer: " ++ show ticks
  display $ map (advance ticks) robots

display :: Input -> IO ()
display rs = do
  let [boardCol, boardRow] = boardSize
  let extractDots = (\[x, y] -> ((y, x), "#")) . head
  let mapping = M.fromList $ map extractDots rs
  forM_ [0 .. boardRow] $ \row -> do
    forM_ [0 .. boardCol] $ \col -> do
      putStr $ M.findWithDefault "." (row, col) mapping
    putStrLn ""

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
      solve2 input
