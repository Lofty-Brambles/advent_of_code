{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Control.Arrow as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as F
import Debug.Trace (traceShowId)
import System.Environment (getArgs)

type Coords = (Int, Int) -- ( y, x )
data Board = Board
  { look :: M.Map Coords Char
  , bot :: Coords
  }

instance Show Board where
  show :: Board -> String
  show board = L.intercalate "\n" grid ++ "\n\nBot @ " ++ botPlace
    where
      grid = L.transpose . chunks (fst ends + 1) . map snd $ M.toAscList merged
      merged = foldr (uncurry M.insert) empty elements
      empty = M.fromList $ [((x, y), '.') | x <- [0 .. fst ends], y <- [0 .. snd ends]]
      ends = fst $ head elements
      elements = M.toDescList $ look board
      botPlace = show $ bot board
      chunks _ [] = []
      chunks n x = (\(ys, zs) -> ys : chunks n zs) $ splitAt n x

type Input = (Board, String)

type Solution = Int

parser :: String -> Input
parser x = (board, concat splitMoves)
  where
    (rawBoard, splitMoves) = break null $ lines x
    markedBoard =
      [ ((x, y), c)
      | (y, line) <- zip [0 ..] rawBoard
      , (x, c) <- zip [0 ..] line
      , c /= '.'
      ]
    board =
      Board
        { look = M.fromList markedBoard
        , bot = fst . F.fromJust $ L.find ((== '@') . snd) markedBoard
        }

-- solve1 :: Input -> Solution
-- solve1 = sum . F.mapMaybe mayGetValue . M.toList . look . moveAll

solve1 :: Input -> Board
solve1 = moveAll

mayGetValue :: (Coords, Char) -> Maybe Int
mayGetValue ((x, y), 'O') = Just (100 * x + y)
mayGetValue (_, _) = Nothing

moveAll :: Input -> Board
moveAll (board, shifts) = foldl move board shifts

move :: Board -> Char -> Board
move board ch =
  if possible == (-1, -1)
    then board
    else traceShowId (board{look = modify $ look board, bot = nextBot})
  where
    modify = M.delete (bot board) . M.insert nextBot '@' . moveLoad
    moveLoad = if nextBot /= possible then M.insert possible 'O' else id
    nextBot = shift ch $ bot board
    possible = traceShowId $ movables $ bot board
    movables pointer = case look board M.!? next of
      Just 'O' -> movables next
      Just '#' -> (-1, -1)
      Nothing -> pointer
      where
        next = traceShowId $ shift ch pointer

shift :: Char -> Coords -> Coords
shift '^' = C.second pred
shift 'v' = C.second succ
shift '<' = C.first pred
shift _ = C.first succ

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
