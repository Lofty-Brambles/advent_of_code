module Main where

import qualified Data.Bits as B
import qualified Data.Char as C
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Data.Maybe as F
import System.Environment (getArgs)

type Register = I.IntMap Int
data Program = Program
  { getPointer :: Int
  , getRegister :: Register
  , getOutput :: [Int]
  , getProgram :: [Int]
  }
  deriving (Show, Eq)

type Instruction = Int -> Program -> Program

type Input = Program

type Solution1 = String
type Solution2 = Int

parser :: String -> Input
parser x = Program 0 (I.fromList $ zip [0 ..] [a, b, c]) [] p
  where
    [[a], [b], [c], _, p] = map (map (read @Int) . extract) $ lines x
    extract = filter (all numCheck) . L.groupBy (\a b -> numCheck a && numCheck b)
    numCheck x = C.isDigit x || (x == '-')

solve1 :: Input -> Solution1
solve1 = L.intersperse ',' . map C.intToDigit . reverse . getOutput . run

run :: Program -> Program
run program
  | getPointer program >= length programGet = program
  | otherwise = run $ instruction operator program
  where
    instruction = instructions I.! (programGet !! pointerGet)
    operator = programGet !! (pointerGet + 1)
    programGet = getProgram program
    pointerGet = getPointer program

instructions :: I.IntMap Instruction
instructions =
  I.fromList $ zip [0 ..] [adv, bxl, bst, jnz, bxc, out, bdv, cdv]

adv :: Instruction
adv op p = delPointer p{getRegister = getRegister'}
  where
    getRegister' = flip (I.insert 0) r $ (r I.! 0) `B.shiftR` combo op r
    r = getRegister p

bxl :: Instruction
bxl op p = delPointer p{getRegister = getRegister'}
  where
    getRegister' = flip (I.insert 1) r $ (r I.! 1) `B.xor` op
    r = getRegister p

bst :: Instruction
bst op p = delPointer p{getRegister = getRegister'}
  where
    getRegister' = flip (I.insert 1) r $ combo op r B..&. 7
    r = getRegister p

jnz :: Instruction
jnz op p = p{getPointer = pointer'}
  where
    pointer'
      | getRegister p I.! 0 == 0 = 2 + getPointer p
      | otherwise = op

bxc :: Instruction
bxc _ p = delPointer p{getRegister = getRegister'}
  where
    getRegister' = flip (I.insert 1) r $ (r I.! 1) `B.xor` (r I.! 2)
    r = getRegister p

out :: Instruction
out op p = delPointer p{getOutput = value : getOutput p}
  where
    value = combo op (getRegister p) B..&. 7

bdv :: Instruction
bdv op p = delPointer p{getRegister = getRegister'}
  where
    getRegister' = flip (I.insert 1) r $ (r I.! 0) `B.shiftR` combo op r
    r = getRegister p

cdv :: Instruction
cdv op p = delPointer p{getRegister = getRegister'}
  where
    getRegister' = flip (I.insert 2) r $ (r I.! 0) `B.shiftR` combo op r
    r = getRegister p

combo :: Int -> Register -> Int
combo op register
  | op B..&. 4 == 0 = op
  | op == 4 = register I.! 0
  | op == 5 = register I.! 1
  | op == 6 = register I.! 2
  | otherwise = error "invalid"

delPointer :: Program -> Program
delPointer program = program{getPointer = getPointer program + 2}

solve2 :: Input -> Solution2
solve2 p = minimum . F.fromJust . reverseEngg p 0 [] . reverse $ getProgram p

reverseEngg :: Program -> Int -> [Int] -> [Int] -> Maybe [Int]
reverseEngg _ r _ toGo
  | null toGo = Just [r]
reverseEngg p r gone (x : toGo)
  | null r' = Nothing
  | otherwise = Just . concat $ F.mapMaybe (\t -> reverseEngg p t gone' toGo) r'
  where
    gone' = x : gone
    r' = filter ((== gone') . reverse . getOutput . run . setA p) shifts
    setA p val = p{getRegister = I.insert 0 val . getRegister $ p}
    shifts = map (B.shiftL r 3 +) [0 .. 7]

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
