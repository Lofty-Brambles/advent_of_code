module Main where

import Control.Arrow (first, second)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)

type Coords = (Int, Int)
type Visited = S.Set Coords

type Input = M.Map Coords Char

type Solution = Int

parser :: String -> Input
parser input = M.fromList [((x, y), c) | (y, line) <- splitLines, (x, c) <- number line]
  where
    splitLines = number $ lines input
    number = zip [0 ..]

solve1 :: Input -> Solution
solve1 = sum . map ((*) <$> length <*> farmPerimeter) . groupFarms

farmPerimeter :: [Coords] -> Int
farmPerimeter points = run points
  where
    check = S.fromList points
    run [] = 0
    run (x : xs) = 4 - length adjacent + run xs
      where
        adjacent = filter (`S.member` check) $ possibleDirs x

groupFarms :: Input -> [[Coords]]
groupFarms mapping
  | M.null mapping = []
  | otherwise = points : groupFarms newMapping
  where
    points = connectFarm mapping point char
    newMapping = foldr M.delete mapping points
    (point, char) = head . M.toList $ M.take 1 mapping

connectFarm :: Input -> Coords -> Char -> [Coords]
connectFarm mapping start farm = visit [start] S.empty
  where
    visit [] _ = []
    visit (x : xs) visited
      | x `S.member` visited = visit xs visited
      | currentFarm /= farm = visit xs visited
      | otherwise = x : visit (xs <> newPoints) (S.insert x visited)
      where
        currentFarm = mapping M.! x
        newPoints = adjacentDirs mapping visited x

adjacentDirs :: Input -> Visited -> Coords -> [Coords]
adjacentDirs mapping visited = filter visits . possibleDirs
  where
    visits other = other `S.notMember` visited && other `M.member` mapping

possibleDirs :: Coords -> [Coords]
possibleDirs pos = map (add pos) diffs
  where
    diffs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

solve2 :: Input -> Solution
solve2 = sum . map ((*) <$> length <*> getSides) . groupFarms
  where
    getSides farm = sides snd False farm encased + sides fst True farm encased
      where
        encased = encase farm

sides :: ((Int, Int) -> Int) -> Bool -> [Coords] -> [Coords] -> Int
sides kind is farmPoints points =
  sum (map (((+) <$> runThru pred <*> runThru succ) . (`getL` points)) l)
  where
    l = L.nub $ map kind points
    getL l = L.sort . filter ((== l) . kind)
    crtl is = if is then first else second
    runThru parser = length . run . filter (\p -> crtl is parser p `elem` farmPoints)
    run [] = []
    run (x : xs) = (x : tailBits) : run (flip drop xs $ length tailBits)
      where
        tailBits = takeWhile (`elem` xs) $ drop 1 $ iterate (crtl (not is) succ) x

encase :: [Coords] -> [Coords]
encase points = S.toList $ check points S.empty
  where
    check [] _ = S.empty
    check (x : xs) visited
      | x `S.member` visited = check xs visited
      | length (adjCheck elem) == 4 = check xs newVisited
      | otherwise = S.fromList (adjCheck notElem) <> check xs newVisited
      where
        newVisited = S.insert x visited
        adjCheck check = filter (`check` points) adjacent
        adjacent = possibleDirs x

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
