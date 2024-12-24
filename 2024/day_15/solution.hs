{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

module Main where

-- debug and improve p2

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import System.Environment (getArgs)

type Coords = (Int, Int) -- ( y, x )
data Board = Board
  { look :: M.Map Coords Char
  , bot :: Coords
  }

instance Show Board where
  show b = L.intercalate "\n" grid ++ "\n\nBot @ " ++ show (bot b)
    where
      grid = chunks (fst ends + 1) . map snd $ M.toAscList merged'
      merged' = M.insert (bot b) '@' merged
      merged = foldr (uncurry M.insert) empti elements
      empti = M.fromList $ [((y, x), '.') | x <- [0 .. fst ends], y <- [0 .. snd ends]]
      elements = M.toDescList $ look b
      ends = (20, 20)
      chunks _ [] = []
      chunks n x = (\(ys, zs) -> ys : chunks n zs) $ splitAt n x

type Input = (Board, String)

type Solution = Int

parser :: String -> Input
parser x = (board, concat splitMoves)
  where
    (rb, splitMoves) = break null $ lines x
    markedBoard =
      [((y, x), c) | (y, l) <- zip [0 ..] rb, (x, c) <- zip [0 ..] l]
    board =
      Board
        { look = M.fromList $ filter (\(_, c) -> c `elem` "#O") markedBoard
        , bot = fst . fromJust $ L.find ((== '@') . snd) markedBoard
        }

sumUp :: Board -> Int
sumUp = M.foldrWithKey' stack 0 . look
  where
    stack (y, x) 'O' t = t + 100 * y + x
    stack (y, x) '[' t = t + 100 * y + x
    stack _ _ t = t

moveBot :: Board -> Char -> Board
moveBot b ch = case move b ch $ shift ch $ bot b of
  Just b' -> b'{bot = shift ch $ bot b}
  Nothing -> b

shift :: Char -> Coords -> Coords
shift '^' (y, x) = (y - 1, x)
shift 'v' (y, x) = (y + 1, x)
shift '<' (y, x) = (y, x - 1)
shift '>' (y, x) = (y, x + 1)

move :: Board -> Char -> Coords -> Maybe Board
move b ch l = case M.lookup l $ look b of
  Nothing -> Just b
  Just '#' -> Nothing
  Just 'O' -> do
    let l' = shift ch l
    b' <- move b ch l'
    return b'{look = M.insert l' 'O' $ M.delete l $ look b'}
  Just '[' -> if ch `elem` "^v" then vertical '>' else horizontal '>'
  Just ']' -> if ch `elem` "^v" then vertical '<' else horizontal '<'
  where
    vertical ch' = do
      let l' = shift ch l
          l'' = shift ch' l'
          l''' = shift ch' l
          br = braces ch'
      b' <- move b ch l''
      b'' <- move b' ch l'
      let insertB = M.insert l' br . M.insert l'' (opp br)
      return b''{look = insertB $ M.delete l $ M.delete l''' $ look b''}
    horizontal ch' = do
      let l' = shift ch' l
          l'' = shift ch' l'
          br = braces ch'
      b' <- move b ch' l''
      let insertB = M.insert l' br . M.insert l'' (opp br)
      return b'{look = insertB $ M.delete l $ look b'}
    braces br = if br == '>' then '[' else ']'
    opp '[' = ']'
    opp ']' = '['

expand :: Input -> Input
expand (b, s) = (Board{look = look', bot = bot'}, s)
  where
    look' = foldr addPoint M.empty . M.toList $ look b
    addPoint (co, '#') = insert2x co '#' '#'
    addPoint (co, 'O') = insert2x co '[' ']'
    insert2x (y, x) a b = M.insert (y, 2 * x) a . M.insert (y, 2 * x + 1) b
    bot' = (\(y, x) -> (y, 2 * x)) $ bot b

solve1 :: Input -> Solution
solve1 = sumUp . uncurry (L.foldl' moveBot)

solve2 :: Input -> Solution
solve2 = sumUp . uncurry (L.foldl' moveBot) . expand

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