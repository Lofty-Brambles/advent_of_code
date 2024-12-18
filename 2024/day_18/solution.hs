module Main where

import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Data.Map as M
import System.Environment (getArgs)

type Coords = (Int, Int)

data Maze = Maze
  { corrupted :: Int -> [Coords]
  , limits :: Int
  , takeable :: Int
  }
data Graph = Graph
  { vertices :: [Coords]
  , neighbours :: Coords -> [Coords]
  }
data DijkData = DijkData
  { dist :: !(M.Map Coords Int)
  , pqueue :: !(PQueue Coords)
  }

type Input = Maze

type Solution1 = Maybe Int
type Solution2 = Coords

parser :: String -> Input
parser x = Maze{corrupted = (`take` corrupted'), limits, takeable}
  where
    corrupted' = take takeable . map wrapTuple $ lines x
    wrapTuple t = read $ "(" ++ t ++ ")"
    limits = 70 -- 6 --
    takeable = length $ lines x -- 1024 -- 12 --

solve1 :: Input -> Solution1
solve1 maze = M.lookup (limits maze, limits maze) $ dist final
  where
    final = dijkstra (mkGraph maze) (0, 0)

mkGraph :: Maze -> Graph
mkGraph maze =
  Graph
    { vertices = [(x, y) | x <- [0 .. l], y <- [0 .. l], unc (x, y)]
    , neighbours
    }
  where
    neighbours v = [v' | v'@(x, y) <- move v, unc v' && check x l && check y l]
    move (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    check d axis = 0 <= d && d <= axis
    unc x = x `L.notElem` corrupted maze (takeable maze)
    l = limits maze

dijkstra :: Graph -> Coords -> DijkData
dijkstra graph start =
  keepLooping
    DijkData
      { dist = M.singleton start 0
      , pqueue = I.singleton 0 [start]
      }
  where
    keepLooping x = case pop $ pqueue x of
      Nothing -> x
      Just (u, pq) -> if maxBound > delta x u then keepLooping x' else x
        where
          x' = L.foldl' (up u $ delta x u) x{pqueue = pq} (neighbours graph u)
    up u du i state = case compare (du + 1) (delta i state) of
      GT -> i
      LT -> DijkData (M.insert state (du + 1) $ dist i) (pAdd $ pqueue i)
      EQ -> DijkData (M.insert state (du + 1) $ dist i) (pqueue i)
      where
        pAdd = addWithPriority state (du + 1)

delta :: DijkData -> Coords -> Int
delta x pos = M.findWithDefault maxBound pos $ dist x

type PQueue a = I.IntMap [a]

pop :: PQueue a -> Maybe (a, PQueue a)
pop pq = case I.lookupMin pq of
  Just (_, []) -> Nothing
  Just (_, x : xs) -> Just (x, pq')
    where
      pq' = if null xs then I.deleteMin pq else I.updateMin getxs pq
      getxs _ = Just xs
  _ -> Nothing

addWithPriority :: a -> Int -> PQueue a -> PQueue a
addWithPriority x p = I.insertWith (++) p [x]

solve2 :: Input -> Solution2
solve2 maze = binarySearch 1025 (length lst + 1)
  where
    lst = corrupted maze (takeable maze)
    binarySearch low high
      | low >= high = lst !! min low high
      | otherwise = case solve1 maze{takeable = mid} of
          Nothing -> binarySearch low (mid - 1)
          Just _ -> binarySearch (mid + 1) high
      where
        mid = (low + high) `div` 2

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
