module Main where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
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
      [((y, x), c) | (y, l) <- zip [0 ..] rb, (x, c) <- zip [0 ..] l]
    board =
      Board
        { look = M.fromList $ filter (\(_, c) -> c `elem` "#O[]") markedBoard
        , bot = fst . fromJust $ L.find ((== '@') . snd) markedBoard
        }

sumUp :: Board -> Int
sumUp = M.foldrWithKey' stack 0 . look
  where
    stack (x, y) 'O' t = t + 100 * x + y
    stack _ _ t = t

moveBot :: Board -> Char -> Board
moveBot b ch = case move b ch $ shift ch $ bot b of
  Just b' -> b'{bot = shift ch $ bot b}
  Nothing -> b

shift :: Char -> Coords -> Coords
shift '^' (x, y) = (y - 1, x)
shift 'v' (x, y) = (y + 1, x)
shift '<' (x, y) = (y, x - 1)
shift '>' (x, y) = (y, x + 1)

move :: Board -> Char -> Coords -> Maybe Board
move b ch l = case M.lookup l $ look b of
  Nothing -> Just b
  Just '#' -> Nothing
  Just 'O' -> do
    let l' = shift ch l
    b' <- move b ch l'
    return b'{look = M.insert l' 'O' $ M.delete l $ look b'}

solve1 :: Input -> Solution
solve1 = sumUp . uncurry (L.foldl' moveBot)

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