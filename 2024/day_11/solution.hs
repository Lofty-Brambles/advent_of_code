module Main where

import qualified Data.MultiSet as DM
import System.Environment (getArgs)

-- ~~cash~~ cache
type Stones = DM.MultiSet Int

type Input = Stones

type Solution = DM.Occur

parser :: String -> Input
parser = DM.fromList . map (read @Int) . words

blink :: Stones -> Stones
blink s = foldr (collatz s) DM.empty $ DM.distinctElems s

collatz :: Stones -> Int -> Stones -> Stones
collatz base value stones
  | value == 0 = DM.insertMany 1 count stones
  | even splitter = foldr (`DM.insertMany` count) stones split
  | otherwise = DM.insertMany (2024 * value) count stones
  where
    count = DM.occur value base
    splitter = ceiling . logBase 10 . fromIntegral $ 1 + value
    split = tupleToList $ value `divMod` (10 ^ div splitter 2)
    tupleToList (x, y) = [y, x]

solve1 :: Input -> Solution
solve1 = DM.size . (!! 25) . iterate blink

solve2 :: Input -> Solution
solve2 = DM.size . (!! 75) . iterate blink

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
