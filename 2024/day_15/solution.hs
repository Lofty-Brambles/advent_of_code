module Main where

import qualified Control.Arrow as CA
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import System.Environment (getArgs)

type Coords = (Int, Int) -- ( y, x )
data Board = Board
  { look :: M.Map Coords Char
  , bot :: Coords
  }

type Input = (Board, String)

type Solution = Int

parser :: String -> Input
parser x = (board, concat splitMoves)
  where
    (rb, splitMoves) = break null $ lines x
    markedBoard =
      [((y, x), c) | (y, l) <- zip [0 ..] rb, (x, c) <- zip [0 ..] l, c /= '.']
    board =
      Board
        { look = M.fromList markedBoard
        , bot = fst . fromJust $ L.find ((== '@') . snd) markedBoard
        }

solve1 :: Input -> Solution
solve1 x = 0

shift :: Char -> Coords -> Coords
shift '^' = CA.first pred
shift 'v' = CA.first succ
shift '<' = CA.second pred
shift _ = CA.second succ

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