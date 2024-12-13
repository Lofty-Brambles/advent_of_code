module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import System.Environment (getArgs)

type Unit = ((Int, Int, Int, Int), (Int, Int))

type Input = [Unit]

type Solution = Int

-- Linear system
-- a b     x   sx
--     (*)   =
-- c d     y   sy

parser :: String -> Input
parser = map (arrange . extract . concat) . chunks 4 . lines
  where
    arrange = (\(a : c : b : d : sx : sy : _) -> ((a, b, c, d), (sx, sy))) . map (read @Int)
    extract = filter (all C.isDigit) . L.groupBy (\a b -> C.isDigit a && C.isDigit b)
    chunks _ [] = []
    chunks n x = (\(ys, zs) -> ys : chunks n zs) $ splitAt n x

solve1 :: Input -> Solution
solve1 = sum . map countTokens . M.mapMaybe calculate

calculate :: Unit -> Maybe (Int, Int)
calculate ((a, b, c, d), (sx, sy)) =
  if x * a + y * b == sx && x * c + y * d == sy
    then Just (x, y)
    else Nothing
  where
    x = (d * sx - b * sy) `div` discrim
    y = (a * sy - c * sx) `div` discrim
    discrim = a * d - b * c

countTokens :: (Int, Int) -> Int
countTokens (x, y) = 3 * x + y

solve2 :: Input -> Solution
solve2 = sum . map countTokens . M.mapMaybe offsetCalculate
  where
    offsetCalculate (i, (sx, sy)) = calculate (i, (offset + sx, offset + sy))
    offset = 10_000_000_000_000

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
