module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S

import qualified Control.Parallel.Strategies as Strat
import System.Environment (getArgs)

type Coords = (Int, Int)
type Direction = Coords
type Bounds = Coords

type Blocks = S.Set Coords
type Pointer = (Coords, Direction)
type Visited = M.Map Coords [Direction]

type Input = (Pointer, Blocks, Bounds)

type Solution = Int

parser :: String -> Input
parser input = (initPosition, blockades, bounds)
  where
    initPosition = (foldr (add . fst) (0, 0) rivettedCoords, (-1, 0))
    blockades = S.fromList $ concatMap snd rivettedCoords
    bounds = (length (head rows) - 1, length rows - 1)
    rivettedCoords = rivetCoords rows
    rows = lines input

rivetCoords :: [String] -> [(Coords, [Coords])]
rivetCoords = zipWith mergeY ([0 ..] :: [Int]) . map loop
  where
    mergeY y (carat, xPos) = (caratPresent (y, carat), [(y, c) | c <- xPos])
    caratPresent v = if snd v == 0 then (0, 0) else v
    loop x = parseLoop 0 x 0 []

parseLoop :: Int -> [Char] -> Int -> [Int] -> (Int, [Int])
parseLoop counter row carat blocks
  | null row = (carat, blocks)
  | head row == '#' = restLoop carat (counter : blocks)
  | head row == '^' = restLoop counter blocks
  | otherwise = restLoop carat blocks
  where
    restLoop = parseLoop (counter + 1) (tail row)

add :: Coords -> Coords -> Coords
add (x, y) (u, v) = (x + u, y + v)

outOfBounds :: Coords -> Coords -> Bool
outOfBounds (bx, by) (x, y) = x < 0 || y < 0 || x > bx || y > by

solve1 :: Input -> Solution
solve1 input = M.size . (\(_, x, _) -> x) $ patrol input

patrol :: Input -> (Pointer, Visited, [Coords])
patrol (pointer, blocks, bounds) = until condition loopBody initial
  where
    loopBody = step blocks
    condition = done bounds
    initial = (pointer, M.empty, [])

done :: Bounds -> (Pointer, Visited, [Coords]) -> Bool
done b (guard, rep, _) = outOfBounds b (fst guard) || cycles rep guard
  where
    cycles visited (pos, dir)
      | Maybe.isNothing isInVisited = False
      | otherwise = dir `elem` Maybe.fromJust isInVisited
      where
        isInVisited = visited M.!? pos

step :: Blocks -> (Pointer, Visited, [Coords]) -> (Pointer, Visited, [Coords])
step blocks (guard@(pos, dir), rep, track) = force (guard', rep', track')
  where
    force x = guard' `seq` rep' `seq` track' `seq` x
    track' = pos : track
    rep' = M.insertWith (++) pos [dir] rep
    guard'
      | next `S.member` blocks = (pos, turn dir)
      | otherwise = (next, dir)
      where
        next = uncurry add guard

turn :: Direction -> Direction
turn (-1, 0) = (0, 1)
turn (0, 1) = (1, 0)
turn (1, 0) = (0, -1)
turn (0, -1) = (-1, 0)

solve2 :: Input -> Solution
solve2 input@(pointer, blocks, bound) = length $ filter id $ parallelised positions
  where
    parallelised = Strat.parMap Strat.rpar pred
    positions = init . L.nub . (\(_, _, x) -> x) $ patrol input
    pred l = do
      let blocks' = S.insert l blocks
      let (pointer', _, _) = patrol (pointer, blocks', bound)
      not $ outOfBounds bound $ fst pointer'

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
