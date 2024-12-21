module Main where

import qualified Data.Map as M
import System.Environment (getArgs)

type Coords = (Int, Int)
type CoordTp = (Int, Int, Bool)
type Pad = M.Map Char Coords

type Input = [String]

type Solution = Int

parser :: String -> Input
parser = lines

processDigits :: Int -> [String] -> Int
processDigits dirPads = foldr foldCodes 0
  where
    foldCodes c t = (+ t) . mult c . loopOver 0 . steps c 1 $ fst pads
    loopOver n result
      | n - 1 == dirPads = result
      | otherwise = loopOver (n + 1) $ eval result
    eval result = foldr (M.unionWith (+)) M.empty (M.mapWithKey stepsOne result) -- problem here?
    stepsOne (x, y, avoid) incr = steps path incr (snd pads)
      where
        path = if avoid then reverse pathBase ++ "A" else pathBase ++ "A"
        neg x = x * (-1)
        pathBase =
          concat
            ( replicate (neg x) '<'
                : replicate y 'v'
                : replicate (neg y) '^'
                : [replicate x '>']
            )

mult :: String -> M.Map CoordTp Int -> Int
mult chars counts = (sum . M.elems $ counts) * (reader . take 3 $ chars)
  where
    reader x = read x :: Int

steps :: String -> Int -> Pad -> M.Map CoordTp Int
steps str incr padKind = (\(x, _, _) -> x) $ foldl acc inits str
  where
    acc (m, (px, py), sp@(spx, spy)) c = do
      let next@(nx, ny) = padKind M.! c
          avoid = nx == spx && py == spy || ny == spy && px == spx
          (dx, dy) = (nx - px, ny - py)
      (M.insertWith (+) (dx, dy, avoid) incr m, next, sp)
    inits = (M.empty, padKind M.! 'A', padKind M.! ' ')

pads :: (Pad, Pad)
pads = (mkPad "789456123 0A", mkPad " ^A<v>")
  where
    mkPad = M.fromList . zipWith flippr [0 ..]
    flippr i c = (c, (\(x, y) -> (y, x)) (i `divMod` 3))

solve1 :: Input -> Solution
solve1 = processDigits 2

solve2 :: Input -> Solution
solve2 = processDigits 25

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
