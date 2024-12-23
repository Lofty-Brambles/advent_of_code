module Main where

import Data.Bifunctor (second)
import Data.Bits ((.&.), (.<<.), (.>>.))
import Data.Char (chr, ord)
import Data.Function (on)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Set as S
import System.Environment (getArgs)

type AdjMatrix = M.IntMap IS.IntSet

type Input = AdjMatrix

type Solution1 = Int
type Solution2 = String

parser :: String -> Input
parser x =
  M.unionsWith (<>) [M.fromList a | a <- formBips . lines $ x]
  where
    formBips = map (uncurry pairUp . second (drop 1) . splitAt 2)
    pairUp x y = [encoder x y, encoder y x]
    encoder x y = (encode x, IS.singleton $ encode y)
    encode [x, y] = ord x .<<. 8 + ord y

decode :: Int -> String
decode x = map chr [x .>>. 8, x .&. 255]

solve1 :: Input -> Solution1
solve1 x = sz . map (triangles x) . filter checkT . M.keys $ x
  where
    sz = S.size . S.unions
    checkT = (== ord 't') . (.>>. 8)

triangles :: AdjMatrix -> Int -> S.Set IS.IntSet
triangles x ka = S.unions . map next . IS.toList $ x M.! ka
  where
    next kb = S.fromList . collect kb . notback . IS.toList $ x M.! kb
    collect kb = map (IS.fromList . (: [kb, ka]))
    notback = filter (IS.member ka . (x M.!))

-- solve2 :: Input -> Solution
solve2 :: Input -> Solution2
solve2 = L.intercalate "," . arrange . getMax . findMaxCliques
  where
    arrange = map decode . IS.toAscList
    getMax = L.maximumBy (compare `on` IS.size)

findMaxCliques :: AdjMatrix -> [IS.IntSet]
findMaxCliques x = go IS.empty (IS.fromList $ M.keys x) IS.empty
  where
    go clique candidates nots =
      case IS.minView candidates of
        Nothing -> [clique | IS.size nots == 0]
        Just (v, remaining) ->
          go
            (IS.insert v clique)
            (IS.intersection candidates $ x M.! v)
            (IS.intersection nots $ x M.! v)
            <> go clique remaining (IS.insert v nots)

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
