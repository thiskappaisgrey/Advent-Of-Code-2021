module Main where
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TO
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
main = do
  input <- TO.readFile "./input1"
  let a = map (T.unpack) $ T.lines input
  let b = map (map (read . pure :: Char -> Int)) a
  let c =   calcGandE $ map (mostLeastCommon 0 0) (matrixTranspose b)
  print (fst c * snd c)
