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
main = do
  input <- TO.readFile "./input"
  let a = (map (read . T.unpack) $ T.lines input) :: [Int]
  print  $ calcIncreasing (tail a) (head a) 0
