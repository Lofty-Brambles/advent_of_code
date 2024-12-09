-- DEFINITELY to be improved.
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import System.Environment (getArgs)

type Index = Int
type FileID = Int

type Input = M.Map Index FileID

type Solution = Int

parser :: String -> Int -> Int -> Bool -> Input -> Input
parser [] _ _ _ input = input
parser (x : xs) index id isFile input
  | isFile = builder (succ id) False (input `M.union` M.fromList indices)
  | otherwise = builder id True input
  where
    builder = parser xs (index + read @Int [x])
    indices = map (,id) $ flip take [index ..] $ read @Int [x]

solve1 :: Input -> Solution
solve1 input = checkSum . moveAll input . maximum $ M.keys input

checkSum :: M.Map Int Int -> Int
checkSum = sum . map (uncurry (*)) . M.assocs

moveAll :: Input -> Int -> Input
moveAll input maxIn = case maybeMoveOne input maxIn of
  Just result -> uncurry moveAll result
  Nothing -> input

maybeMoveOne :: Input -> Int -> Maybe (Input, Int)
maybeMoveOne input maxIn = do
  freeSpaces <- L.find (`M.notMember` input) [0 .. maxIn]
  moveableIDs <- M.lookup maxIn input
  let removeOld = M.delete maxIn input
      newMapping = M.insert freeSpaces moveableIDs removeOld
      newMaxIndex = until (`M.member` newMapping) pred maxIn
  pure (newMapping, newMaxIndex)

solve2 :: Input -> Solution
solve2 = checkSum . moveAllV2

moveAllV2 :: Input -> Input
moveAllV2 input = loop [maxID, maxID - 1 .. 0] input
  where
    maxID = maximum $ M.elems input
    maxIndexFor mapping x = maximum . map fst $ M.toList $ M.filter (== x) mapping
    loop [] mapping = mapping
    loop (x : xs) mapping = case tryMoving mapping x $ maxIndexFor mapping x of
      Just newMapping -> loop xs newMapping
      Nothing -> loop xs mapping

tryMoving :: Input -> Int -> Int -> Maybe Input
tryMoving mapping currentID maxIndex = do
  let indices = M.keys $ M.filter (== currentID) mapping
      len = length indices
  freeSpaceWorks <- findFittingFreeSpace mapping len maxIndex
  let removedOld = foldr M.delete mapping indices
      start = minimum freeSpaceWorks
      range = [start .. start + len - 1]
  pure $ foldr (`M.insert` currentID) removedOld range

findFittingFreeSpace :: Input -> Int -> Int -> Maybe [Int]
findFittingFreeSpace = loop 0
  where
    loop start mapping len maxIndex
      | start > maxIndex = Nothing
      | otherwise = do
          let indices = [start .. maxIndex]
          freeSpaceStart <- L.find (`M.notMember` mapping) indices
          freeSpaceEnd <- L.find (`M.member` mapping) . drop (freeSpaceStart - start) $ indices
          let freeLen = freeSpaceEnd - freeSpaceStart
          if freeLen >= len
            then Just [freeSpaceStart .. freeSpaceEnd - 1]
            else loop freeSpaceEnd mapping len maxIndex

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- shift parser 0 0 True M.empty <$> readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
  where
    shift f a b c d x = f x a b c d
