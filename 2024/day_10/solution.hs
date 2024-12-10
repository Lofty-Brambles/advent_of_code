module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)

type Coords = (Int, Int)
type CoordSet = S.Set Coords

type Input = (Coords, M.Map Int [Coords])

type Solution = Int

parser :: String -> Input
parser input = ((length rows, length $ head rows), mapping)
  where
    mapping = fst . foldl lineFoldr (M.empty, 0) $ rows
    lineFoldr (m, y) line = (fst3 $ foldl charMapper (m, 0, y) line, succ y)
    charMapper (m, x, y) char = (addToMap m char x y, succ x, y)
    addToMap m char x y = M.insertWith (++) (read [char]) [(x, y)] m
    rows = lines input
    fst3 (x, _, _) = x

solve1 :: Input -> Solution
solve1 i = finder (\x -> length $ iterTrails i 1 $ S.fromList [x]) i

finder :: (Coords -> Int) -> Input -> Solution
finder appl input@(_, mapping) = sum $ map appl $ mapping M.! 0

iterTrails :: Input -> Int -> CoordSet -> CoordSet
iterTrails _ 10 positions = positions
iterTrails i@(bound, mapping) lvl positions = iterTrails i (succ lvl) newPos
  where
    newPos = S.fromList $ concatMap (filter isValid . possibleDirs bound) positions
    isValid position = position `elem` mapping M.! lvl

possibleDirs :: Coords -> Coords -> [Coords]
possibleDirs bounds pos = filter (checkBounds bounds) $ map (add pos) diffs
  where
    diffs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    checkBounds (bx, by) (x, y) = x >= 0 && y >= 0 && x <= bx && y <= by

solve2 :: Input -> Solution
solve2 i = finder (\x -> length $ iterTrails' i 1 [x]) i

iterTrails' :: Input -> Int -> [Coords] -> [Coords]
iterTrails' _ 10 positions = positions
iterTrails' i@(bound, mapping) lvl positions = iterTrails' i (succ lvl) newPos
  where
    newPos = concatMap (filter isValid . possibleDirs bound) positions
    isValid position = position `elem` mapping M.! lvl

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
