module Main where
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TO
import System.Directory.Internal.Prelude (getArgs)
-- The idea is to take a list of lists like: [[1, 0, 1], [0, 1, 0] ] and transpose it ito a list like: [[1,0], [0,1], [1, 0]]
-- Then, I can write a function to count the most common and least common in a list.
-- Finally, I can convert the binary number into decimal(I need to first reverse the digits and multiply by 2^x or get the lenght and multiply by reverse order?).

-- to perform a transpose, I take the first element of every list, then add it to another list.
-- Given this list: [[1, 0, 1], [0, 1, 0] ]
-- yields [[1,0]] and reduces to [[0, 1], [1, 0] ] in the next recursion step
-- assume that the length of every internal list is the same(which makes this method unsafe)
matrixTranspose ::  [[a]] -> [[a]]
-- check if all elements are empty
matrixTranspose ([]:k) = []
matrixTranspose a = map head a:matrixTranspose (map tail a)

type Max = Int
type Min = Int
-- input is [1,1,0,1]
-- returns (1,0)
-- only works for zeroes and ones
mostLeastCommon ::  Int -> Int -> [Int]  -> (Max, Min)
mostLeastCommon  zeros ones [] = if zeros > ones then (0, 1) else (1, 0)
mostLeastCommon  zeros ones (0:list) = mostLeastCommon (zeros + 1) ones list
mostLeastCommon  zeros ones (1:list) = mostLeastCommon zeros ( ones + 1 ) list
mostLeastCommon  zeros ones (a:list) = mostLeastCommon zeros ones list

-- calculate the gamma and epsilon rate
type Gamma = Int
type Epsilon = Int
calcGandE :: [(Max, Min)] -> (Gamma, Epsilon)
calcGandE list = calcDec rl 0
  where
    rl = reverse list
    pairAdd (a,b) (c,d) = (a + c, b + d)
    calcDec [] acc = (0,0)
    calcDec (a:l) acc = pairAdd (2^acc * (fst a), 2^acc * (snd a)) (calcDec l (acc + 1))
part1Monad = do
  input <- TO.readFile "./input1"
  let a = map (T.unpack) $ T.lines input
  let b = map (map (read . pure :: Char -> Int)) a
  let c =   calcGandE $ map (mostLeastCommon 0 0) (matrixTranspose b)
  print (fst c * snd c)


-- Part 2
-- Part 2 is going to be different than how I did part one. The idea is to put the list of lists into a matrix structure for fast access.
-- Then, filter out the list of row numbers of the "correct" numbers for the matrix

-- The idea for this one is, you start with a matrix like this(say the index is 0): [[1,0,1,1], [0,0,1,1], [1,1,1,0]] , get the 0th element by transforming it into: [1,0,1]
-- Then from that list, get the most common element: 1. Then, filter out the stuff with the least comon bit to end up with [[1,0,1,1], [1,1,1,0]]
filterByBit :: [[Int]] -> Int -> [Int]
filterByBit [a] _ = a
filterByBit matrix i = let
  lWithIthBit = map (!!i) matrix
  mostCommonBit = fst $ mostLeastCommon 0 0 lWithIthBit
  in
  filterByBit  (filter ((== mostCommonBit) . (!! i)) matrix) (i + 1)
-- for min
filterByBit2 :: [[Int]] -> Int -> [Int]
filterByBit2 [a] _ = a
filterByBit2 matrix i = let
  lWithIthBit = map (!!i) matrix
  leastCommonBit = snd $ mostLeastCommon 0 0 lWithIthBit
  in
  filterByBit2  (filter ((== leastCommonBit) . (!! i)) matrix) (i + 1)
binToDec ::   [Int]  -> Int
binToDec list = calcBin 0 $ reverse list
  where
  calcBin _ [] = 0
  calcBin  acc (n:bin) = (2^acc * n) + calcBin (acc +1) bin
part2Monad = do
  name <- getArgs
  input <- TO.readFile $ head name
  let a = map T.unpack $ T.lines input
  let b = map (map (read . pure :: Char -> Int)) a
  -- print b
  print $ binToDec $   filterByBit b 0
  print $ binToDec $   filterByBit2 b 0
main = part2Monad
