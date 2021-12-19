module Main where
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TO

textToInt :: T.Text -> Int
textToInt t =  read $ T.unpack t

calcIncreasing :: [Int] -> Int ->  Int -> Int
-- strategy is to fold a list of text into a single integer
-- However, I need to keep track of the previous value
-- and also the number of "increasings" I see.
calcIncreasing [] prev accum = accum
calcIncreasing (curr:list) prev accum = if curr > prev
    then calcIncreasing list curr (accum + 1)
    else calcIncreasing list curr accum
-- Part 2
-- My idea for this is to take the list and create a list of triples of measurements(how do I do this?)
makeTriple :: [Int] -> [[Int]]
makeTriple [] = []
makeTriple [a] = []
makeTriple [a, b] = []
makeTriple (a:b:c:list) = [a, b, c] : makeTriple (b:c:list)
-- Do a sum over the triples (easy)

-- Then call calcIncreasing
main = do
  input <- TO.readFile "./input4"
  let a = (map (read . T.unpack) $ T.lines input) :: [Int]
  let b = map sum (makeTriple a)
  print  $ calcIncreasing (tail b) (head b) 0
