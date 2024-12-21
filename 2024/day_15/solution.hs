module Main where

import qualified Control.Arrow as CA
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Debug.Trace (traceShowId)

type Coords = (Int, Int) -- ( y, x )
data Board = Board
  { look :: M.Map Coords Char
  , bot :: Coords
  }

type Input = (Board, String)

type Solution = Int

instance Show Board where
  show board = L.intercalate "\n" grid ++ "\n\nBot @ " ++ botPlace
    where
      grid = chunks (fst ends + 1) . map snd $ M.toAscList merged
      merged = foldr (uncurry M.insert) empty elements
      empty = M.fromList $ [((x, y), '.') | x <- [0 .. fst ends], y <- [0 .. snd ends]]
      ends = fst $ head elements
      elements = M.toDescList $ look board
      botPlace = show $ bot board
      chunks _ [] = []
      chunks n x = (\(ys, zs) -> ys : chunks n zs) $ splitAt n x

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
solve1 = sumUp . uncurry (L.foldl' pushBot)

sumUp :: Board -> Int
sumUp b = M.foldrWithKey' recurAdd 0 $ look b
  where
    recurAdd (x, y) 'O' t = 100 * x + y + t
    recurAdd _ _ t = t

pushBot :: Board -> Char -> Board
pushBot b ch = case traceShowId $ pushObjects b ch next of
  Just b' -> b'{bot = next}
  Nothing -> b
  where
    next = shift ch $ bot b

shift :: Char -> Coords -> Coords
shift '^' = CA.first pred
shift 'v' = CA.first succ
shift '<' = CA.second pred
shift _ = CA.second succ

pushObjects :: Board -> Char -> Coords -> Maybe Board
pushObjects b ch p = case M.lookup p $ look b of
  Nothing -> Just b
  Just '#' -> Nothing
  Just 'O' -> do
    let p' = shift ch p
    b' <- pushObjects b ch p'
    return b'{look = M.insert p' 'O' . M.delete p . look $ b'}
  Just _ -> Nothing

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