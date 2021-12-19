module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TO
-- main needs to parse a line of instruction, like:
-- "forward 5"
-- and return "Forward 5" as a type, finally, returing a list of Instructions
-- that I can use to fold into two numbers, a height and a depth
main = do
  input <- TO.readFile "./input3"
  let a = map (words . T.unpack) $ T.lines input
  let b = map stringToInstruction a
  let c = calculatePositonAim b (0,0, 0)
  print  $ fst' c * snd' c


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

-- Part 2 code
type Aim = Int
type PositionAim = (Depth, HorizontalPosition, Aim)

thd (_,_,b) = b
fst' (a, _, _) = a
snd' (_, a, _) = a
calculatePositonAim :: [Instruction] -> PositionAim -> PositionAim
calculatePositonAim ((Forward a):list) pos = calculatePositonAim list (thd pos * a + fst' pos, a + snd' pos, thd pos)
calculatePositonAim ((Up a):list) pos = calculatePositonAim list (fst' pos, snd' pos, thd pos - a)
calculatePositonAim ((Down a):list) pos = calculatePositonAim list (fst' pos, snd' pos, thd pos + a)
calculatePositonAim (None:list) pos = calculatePositonAim list pos
calculatePositonAim [] pos = pos
