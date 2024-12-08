module Main where

import qualified Control.Monad as G
import qualified Data.Set as S
import System.Environment (getArgs)

type Identifier = (Int, Int, Char)
type Bounds = (Int, Int)

type Input = (Bounds, [Identifier])

type Solution = Int

parser :: String -> Input
parser input = ((length rows, length $ head rows), filtered)
  where
    filtered = concatMap (filter (\(_, _, c) -> c /= '.')) identifiers
    identifiers = zipWith (\y -> zipWith (`set` y) [0 ..]) [0 ..] rows
    set x y c = (x, y, c)
    rows = lines input

solve1 :: Input -> Solution
solve1 (bounds, blocks) = length . bind bounds $ antinodes blocks
  where
    antinodes points = do
      (x1, y1, c1) <- points
      (x2, y2, c2) <- points
      G.guard (c1 == c2 && (x2, y2) > (x1, y1))
      let dx = 2 * (x2 - x1)
      let dy = 2 * (y2 - y1)
      [(x1 + dx, y1 + dy, '#'), (x2 - dx, y2 - dy, '#')]

bind :: Bounds -> [Identifier] -> [Identifier]
bind bound = filter inBounds . S.toList . S.fromList
  where
    inBounds (x, y, _) = x >= 0 && y >= 0 && x < fst bound && y < snd bound

solve2 :: Input -> Solution
solve2 (bounds, blocks) = length . bind bounds $ antinodes blocks
  where
    range = [-(fst bounds) .. (fst bounds)]
    antinodes points = do
      (x1, y1, c1) <- points
      (x2, y2, c2) <- points
      n <- range
      G.guard (c1 == c2 && (x2, y2) > (x1, y1))
      let dx = x2 - x1
      let dy = y2 - y1
      [(x1 + n * dx, y1 + n * dy, '#')]

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
