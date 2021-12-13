module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TO
-- main needs to parse a line of instruction, like:
-- "forward 5"
-- and return "Forward 5" as a type, finally, returing a list of Instructions
-- that I can use to fold into two numbers, a height and a depth
main = do
  input <- TO.readFile "./input2"
  let a = map (words . T.unpack) $ T.lines input
  let b = map stringToInstruction a
  let c = calculatePositon b (0,0)
  print  $ fst c * snd c


-- Part 1 code
-- for this problem, I have to somehow keep track of the depth info
-- and also keep track of the current Depth/Horizontal information
data Instruction = Forward Int | Down Int | Up Int | None deriving Show
type Depth = Int
type HorizontalPosition = Int
type Position = (Depth, HorizontalPosition)

stringToInstruction :: [String] -> Instruction
stringToInstruction [a,b] =
  case a of
    "forward" -> Forward (read b)
    "up" -> Up (read b)
    "down" -> Down (read b)
    _ -> None
calculatePositon :: [Instruction] -> Position -> Position
calculatePositon ((Forward a):list) pos = calculatePositon list (fst pos, a + snd pos)
calculatePositon ((Up a):list) pos = calculatePositon list ( fst pos - a, snd pos)
calculatePositon ((Down a):list) pos = calculatePositon list (fst pos + a, snd pos)
calculatePositon (None:list) pos = calculatePositon list pos
calculatePositon [] pos = pos
