module Main where

-- revisit part 2, check it better to make it faster programatically

import Data.Bits ((.&.), (.^.), (.|.))
import qualified Data.Char as C
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)
import System.Environment (getArgs)
import Text.Printf (printf)

type Wires = M.Map String Int
type Directive = (String, String, String, String, Bool)
type DirMap = IM.IntMap Directive

type Input = (Wires, DirMap)

type Solution1 = Int
type Solution2 = String

parser :: String -> Input
parser x = (wires, IM.fromAscList $ zip [0 ..] gates)
  where
    wires = foldr parseWire M.empty wiresStr
    parseWire x = M.insert (take 3 x) (C.digitToInt $ last x)
    gates = map (arrange . extract) gatesStr
    arrange (a : _ : op : _ : b : _ : _ : _ : c : _) = (op, a, b, c, False)
    extract = L.groupBy (\a b -> not (C.isSpace a) && not (C.isSpace b))
    (wiresStr, _ : gatesStr) = break (== "") . lines $ x

solve1 :: Input -> Solution1
solve1 x = convert . map snd $ zees
  where
    zees = filter ((== 'z') . head . fst) . M.assocs . fst $ uncurry simulate x
    convert [] = 0
    convert (x : xs) = x + 2 * convert xs

simulate :: Wires -> DirMap -> Input
simulate wires d
  | all (\(_, _, _, _, b) -> b) $ IM.elems d = (wires, d) -- guard
  | otherwise = uncurry simulate $ loopDirs (wires, d) $ IM.assocs d

loopDirs :: Input -> [(Int, Directive)] -> Input
loopDirs datas [] = datas
loopDirs datas ((_, (_, _, _, _, True)) : xs) = loopDirs datas xs
loopDirs datas@(w, d) ((n, (op, a, b, c, possible)) : xs)
  | not (M.member a w && M.member b w) = loopDirs datas xs
  | otherwise = case op of
      "AND" -> process ((w M.! a) .&. (w M.! b))
      "OR" -> process ((w M.! a) .|. (w M.! b))
      "XOR" -> process ((w M.! a) .^. (w M.! b))
  where
    process x = loopDirs (newW x, newD) xs
    newW x = M.insert c x w
    newD = IM.insert n (op, a, b, c, True) d

-- bkr,mqh,rnq,tfb,vvr,z08,z28,z39
solve2 :: Input -> Solution2
solve2 = L.intercalate "," . L.sort . parallelCheck Nothing [] 0 . snd

parallelCheck :: Maybe String -> [String] -> Int -> DirMap -> [String]
parallelCheck _ swaps 45 _ = reverse swaps
parallelCheck _ s 0 d = parallelCheck (gets d ("AND", "x00", "y00")) s 1 d
parallelCheck cc s n d
  | isNothing cXor = parallelCheck cc (push ab s) 0 (swap d ab)
  | fromJust cXor /= z =
      parallelCheck cc (push [cXor, Just z] s) 0 (swap d [cXor, Just z])
  | otherwise = parallelCheck cc' s (n + 1) d
  where
    cc' = gets d ("OR", fromJust abAnd, fromJust cAnd)
    push x r = map fromJust x ++ r
    (cXor : cAnd : _) = ret (fromJust abXor) (fromJust cc)
    ab@(abXor : abAnd : _) = ret x y
    ret x y = map (\op -> gets d (op, x, y)) ["XOR", "AND"]
    (x : y : z : _) = map (\a -> printf (a : "%02d") n) "xyz" :: [String]

swap :: DirMap -> [Maybe String] -> DirMap
swap d strs = IM.fromList . map checker . IM.toList $ d
  where
    (a : b : _) = map fromJust strs
    checker (index, (op, a, b, c, p)) = (index, (op, a, b, c', p))
      where
        c' = M.findWithDefault c c $ M.fromList [(a, b), (b, a)]

gets :: DirMap -> (String, String, String) -> Maybe String
gets ds (op, a, b) = case L.find matcher $ IM.toAscList ds of
  Just (_, (_, _, _, c, _)) -> Just c
  Nothing -> Nothing
  where
    matcher (_, (op', a', b', c', _)) =
      op == op' && ((a, b) == (a', b') || (a, b) == (b', a'))

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
