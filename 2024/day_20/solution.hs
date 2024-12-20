module Main where

import qualified Data.Map.Lazy as M
import qualified Data.Maybe as F
import qualified Data.Set as S
import System.Environment (getArgs)

type Coords = (Int, Int)
type MazeCosts = M.Map Coords Int
data Maze = Maze
  { walls :: S.Set Coords
  , start :: Coords
  , end :: Coords
  }
  deriving (Eq, Ord, Show)

type Input = (Maze, MazeCosts, MazeCosts)

type Solution = Int

parser :: String -> Input
parser input = (maze, costs maze start, costs maze end)
  where
    costs m t = getCosts m (M.singleton (t m) 0) (M.singleton (t m) 0)
    maze = Maze{walls = S.fromList walls, start = start', end = end'}
    (walls, start', end') = foldr finder ([], (-1, -1), (-1, -1)) coordList
    coordList = [((y, x), c) | (y, l) <- zip [0 ..] rows, (x, c) <- zip [0 ..] l]
    rows = lines input
    finder (coord, ch) old@(m, s, e)
      | ch == '#' = (coord : m, s, e)
      | ch == 'S' = (m, coord, e)
      | ch == 'E' = (m, s, coord)
      | otherwise = old

getCosts :: Maze -> MazeCosts -> MazeCosts -> MazeCosts
getCosts maze costs bound
  | M.null bound = costs
  | otherwise = getCosts maze costs' bound'
  where
    costs' = costs `M.union` foldedBound
    bound' = foldedBound `M.difference` costs
    foldedBound = M.foldlWithKey addBound M.empty bound
    addBound m p cost = M.union m . M.fromList . map (,cost + 1) . xGen maze $ p

xGen :: Maze -> Coords -> [Coords]
xGen maze pos = filter (`S.notMember` walls maze) . map (add pos) $ deltas
  where
    deltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]

add :: Coords -> Coords -> Coords
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

solve1 :: Input -> Solution
solve1 = uncurry3 $ cheat 100 2

cheat :: Int -> Int -> Maze -> MazeCosts -> MazeCosts -> Int
cheat threshold l maze costS costE =
  length . filter (>= threshold) . fmap ((costS M.! end maze) -) $ cheats
  where
    cheats = concat [pathCosts p | p <- M.keys costS]
    pathCosts pos = (+ costS M.! pos) <$> contCosts l maze costS costE pos

contCosts :: Int -> Maze -> MazeCosts -> MazeCosts -> Coords -> [Int]
contCosts l maze costS costE x = F.mapMaybe contCosts numbers
  where
    manhattan (cx1, cy1) (cx2, cy2) = abs (cx1 - cx2) + abs (cy1 - cy2)
    contCosts n = do
      gc <- M.lookup n costE
      let sc = manhattan n x
      return $ gc + sc
    contCosts' n = undefined
    numbers =
      [ add x (dr, dc)
      | dr <- [-l .. l]
      , dc <- [-l .. l]
      , abs dr + abs dc <= l
      ]

solve2 :: Input -> Solution
solve2 = uncurry3 $ cheat 100 20

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
