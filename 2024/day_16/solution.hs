module Main where

import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)

type Coords = (Int, Int)
data Dir = N | W | S | E deriving (Eq, Ord, Enum, Show)
type Reindeer = (Coords, Dir)

data Maze = Maze
  { walls :: S.Set Coords
  , start :: Coords
  , end :: Coords
  , limits :: Coords
  }
data Graph = Graph
  { vertices :: [Reindeer]
  , neighbours :: Reindeer -> [Reindeer]
  , score :: Reindeer -> Reindeer -> Int
  }
data DijkData = DijkData
  { dist :: !(M.Map Reindeer Int)
  , prev :: !(M.Map Reindeer [Reindeer])
  , pqueue :: !(PQueue Reindeer)
  }

type Input = Maze

type Solution = Int

parser :: String -> Input
parser x = Maze{walls = S.fromList filterMaze, start, end, limits}
  where
    maze = [((x, y), c) | (y, l) <- zip [0 ..] $ lines x, (x, c) <- zip [0 ..] l]
    (filterMaze, start, end) = foldr finder ([], (-1, -1), (-1, -1)) maze
    limits = (length $ lines x, length . head $ lines x)
    finder (coord, ch) old@(m, s, e)
      | ch == '#' = (coord : m, s, e)
      | ch == 'S' = (m, coord, e)
      | ch == 'E' = (m, s, coord)
      | otherwise = old

solve1 :: Input -> Solution
solve1 maze = minimum [delta graph (end maze, d) | d <- [N, E, S, W]]
  where
    graph = dijkstra (mkGraph maze) (start maze, E)

mkGraph :: Maze -> Graph
mkGraph maze =
  Graph
    { vertices = [(l, d) | l <- S.toList locations, d <- [N, E, S, W]]
    , neighbours = \r -> [x | (x, _) <- move r, fst x `S.member` locations]
    , score = \r1 r2 -> head [c | (r2', c) <- move r1, r2 == r2']
    }
  where
    locations = S.fromList [(x, y) | x <- gLst fst, y <- gLst snd, check (x, y)]
    gLst kind = [0 .. kind (limits maze) - 1]
    check d = d `S.notMember` walls maze

dijkstra :: Graph -> Reindeer -> DijkData
dijkstra graph start =
  keepLooping
    DijkData
      { dist = M.singleton start 0
      , prev = M.empty
      , pqueue = I.singleton 0 [start]
      }
  where
    keepLooping x = case minView $ pqueue x of
      Nothing -> x
      Just (u, pq) -> if maxBound > delta x u then keepLooping x' else x
        where
          x' = L.foldl' (update u $ delta x u) x{pqueue = pq} (neighbours graph u)
    update u du i state = case compare alt (delta i state) of
      GT -> i
      LT -> DijkData (addS alt $ dist i) (addS [u] $ prev i) (pAdd $ pqueue i)
      EQ -> DijkData (addS alt $ dist i) (stickS [u] $ prev i) (pqueue i)
      where
        stickS = M.insertWith (++) state
        pAdd = addWithPriority state alt
        addS = M.insert state
        alt = du + score graph u state

delta :: DijkData -> Reindeer -> Int
delta x pos = M.findWithDefault maxBound pos $ dist x

move :: Reindeer -> [(Reindeer, Int)]
move (l, d) = ((straight l d, d), 1) : map turns [(+), (-)]
  where
    turns t = ((l, turn t d), 1000)
    turn way dir = toEnum (way 1 (fromEnum dir) `mod` 4)
    straight (x, y) d = case d of
      N -> (x, y - 1)
      S -> (x, y + 1)
      W -> (x - 1, y)
      E -> (x + 1, y)

type PQueue a = I.IntMap [a]

minView :: PQueue a -> Maybe (a, PQueue a)
minView pq = case I.lookupMin pq of
  Just (_, x : xs) -> Just (x, pq')
    where
      pq' = if null xs then I.deleteMin pq else I.updateMin getxs pq
      getxs _ = Just xs
  _ -> Nothing

addWithPriority :: a -> Int -> PQueue a -> PQueue a
addWithPriority x p = I.insertWith (++) p [x]

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
