{-# LANGUAGE OverloadedStrings #-}

-- |
module Main where

import Control.Monad (join)
import Data.Array
import Data.List ( sortBy, foldl' )
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import qualified Data.Text.Read as TR
import System.Environment (getArgs)
import Debug.Trace
import GHC.Arr (foldlElems')
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Bifunctor (first)

-- in day 4, there's actually 2 problems. The problem of parsing a bingo board
-- and the problem of representing a bingo board

-- This is responsible for breaking up the input lines
-- into more sensible chunks
-- Keep track of the internal lists in the third argument
parseInput :: T.Text -> [T.Text] -> [T.Text] -> [[T.Text]]
parseInput _ [] [] = []
parseInput _ [] a = [a]
parseInput c (h : a) b
  | h == c = b : parseInput c a []
  | otherwise = parseInput c a (b ++ [h])

-- Now, I have a list like this: [["1, 2, 3, 4"], ["22 13 ...", "8 2 ..."]] where the head of the list are bingo numbers, and tail is the boards

-- The problem is:
-- given a Bingo board, and list of numbers, find the board that wins first
-- This problem is easier to do with a proper array, because I need to check rows AND columns
-- Classically, I can do this with some sort of looping over the bingo board to "mark" the cell
data Cell = Cell
  { mark :: Bool,
    val :: Int
  }
instance Show Cell where
  show c = if mark c then show (val c) ++ "!" else show (val c)

type Board = Array (Int, Int) Cell

-- cache the data to make it faster to mark boards and such
data BoardInfo = BoardInfo
  {
    indicies :: Array Int ((Int, Int), Cell), -- Keep a sorted list of indicies to make marking easier
    -- Keep track of how many bingo in each row/col
    rows :: Array Int Int,
    cols :: Array Int Int
  } deriving Show

-- emptyBoardInfo :: BoardInfo
-- emptyBoardInfo = BoardInfo {
--   indicies = ,
--   rows = listArray  (1,5) $ replicate 0 5 ,
--   cols = listArray  (1,5) $ replicate 0 5
--   }
-- initialize the board info
-- actually, maybe the sorting thing doesn't matter?? I don't know. I would have to encode the indicies in an array

boardInfo :: Board -> BoardInfo
boardInfo board =
   BoardInfo { indicies = calcIndicies board,
      rows = listArray  (1,5) $ replicate 5 0 ,
      cols = listArray  (1,5) $ replicate 5 0
             }
calcIndicies :: Board -> Array Int ((Int, Int), Cell)
calcIndicies = listArray (1,25) . sortBy (\(_, a) (_, b) -> compare (val a) (val b)) . assocs
-- modified from: https://rosettacode.org/wiki/Binary_search#Haskell
-- This makes the marking more efficient.
indicesBinSearch ::  Int -> (Int, Int) -> Array Int ((Int, Int), Cell)  ->  Maybe ((Int, Int), Cell)
indicesBinSearch  cell (low, high) a
  | low > high = Nothing
  | otherwise =
    let
      mid = (low + high) `div` 2
      i =  a ! mid

      cellval = val $ snd i
    in case compare cell cellval of
      LT -> indicesBinSearch  cell (low, mid - 1) a
      GT -> indicesBinSearch  cell (mid +1, high) a
      EQ -> Just i


-- TODO printBoard:  printArray arr = unlines [unwords [show (arr ! (x, y)) | x <- [1..2]] | y <- [1..2]]

-- TODO mark board takes an initialized board info(not sure how to encode in the type), then mark the board if found
markBoard :: (BoardInfo, Board) -> Int -> (BoardInfo, Board)
markBoard (binf, board) num =
  let
    inds = indicies binf
    maybeCell = indicesBinSearch num (bounds  inds) inds
  in
    case maybeCell of
      Nothing -> (binf, board)
      Just ((row, col), val) ->
        let
          binfUpt = binf {
            --  not sure if there's a cleaner way to write this..
            rows = rows binf // [(row, rows binf !  row + 1)]
            , cols = cols binf // [(col, cols binf ! col + 1)]
                         }
          boardUpt = board // [((row, col), (board ! (row,col)) { mark = True })]
                in
          (binfUpt, boardUpt)

-- A function to mark the board with a list of numbers. Actually need another version of this because I also need the winning number and the winning in..
markBoardList :: Board -> [Int] -> (BoardInfo, Board)
-- Learned something new! I didn't really realize that you can fold values of two different types..
-- foldl' works because of the function signature (b -> a -> b). Foldr wouldn't work here.
-- Here, I'm taking the tuple (BoardInfo, Board), and accumulating it using ints by marking it. Look at the type signature of markBoard.
markBoardList board = mblHelper (boardInfo board, board)
  where
    mblHelper = foldl' markBoard

-- check if board is winning and check the number that was called when board won?
checkBoard :: Board -> Bool
checkBoard board = (or [ checkRow board row | row <- [1..5] ]) || or [ checkCol board col | col <- [1..5] ]
  where
    checkRow :: Board -> Int ->  Bool
    checkRow b row =  and  [mark $ board ! (row, col)  | col <- [1..5] ]
    checkCol :: Board -> Int ->  Bool
    checkCol b col =  and [mark $ board ! (row, col)  | row <- [1..5] ]
-- should be faster than checkBoard
checkBoard' :: BoardInfo -> Bool
checkBoard' binf = c (cols binf) || c (rows binf)
  where
    -- checks if and element in the array is 5
   c =  foldlElems' (\a b -> b == 5  || a) False

-- returns  no of turns and the winning number
boardWinsIn :: Board -> [Int] -> (Maybe (Int, Int), Board)
boardWinsIn board = boardWinsIn' (boardInfo board, board) 0
 where
    boardWinsIn' :: (BoardInfo, Board) -> Int -> [Int] -> (Maybe (Int, Int), Board)
    boardWinsIn' b _ [] = (Nothing, snd b)
    boardWinsIn' b a (hd:tl) =
      let
        newB = markBoard b hd
        win = checkBoard' $ fst newB
      in
      if win then
        (Just (a, hd), snd newB)
      else boardWinsIn' newB (a + 1) tl

sumUnmarked :: Board -> Int
sumUnmarked = foldlElems' (\a b -> a + (if mark b then 0 else val b)) 0



parseBoard :: [T.Text] -> Board
parseBoard =  listArray ((1,1), (5,5)) . join . map parseRow
  where
    parseRow :: T.Text -> [Cell]
    parseRow = map (Cell False . read . T.unpack) . filter (/= "") . T.splitOn " "



testBoard :: String -> IO ()
testBoard file = do
  input <- TO.readFile file
  let a = T.lines input
  let b = parseInput "" a []
  let bingoNumbers = map (read . T.unpack :: T.Text -> Int) $ T.splitOn "," $ head $ head b
  let board =  parseBoard $ b !! 3
  let t = boardWinsIn board bingoNumbers
  print $ fst t
  let sum =  sumUnmarked $ snd t
  print sum
  let score = (* sum) $  snd $ fromMaybe (0,0) $ fst t
  print score

minBoard :: [((Int, Int), Board)] -> Int -> ((Int, Int), Board) -> ((Int, Int), Board)
minBoard [] minTurns b = b
-- minBoard ((Nothing, _):tl) minTurns b = minBoard tl minTurns b
minBoard (((turns, winNum), bn):tl) minTurns b
  | turns < minTurns = minBoard tl turns ((turns, winNum), bn)
  | otherwise  = minBoard tl minTurns b
--  Part 2
-- I can probably factor out  the compare function into the arg. but I'm too lazy
maxBoard :: [((Int, Int), Board)] -> Int -> ((Int, Int), Board) -> ((Int, Int), Board)
maxBoard [] maxTurns b = b
-- minBoard ((Nothing, _):tl) minTurns b = minBoard tl minTurns b
maxBoard (((turns, winNum), bn):tl) maxTurns b
  | turns > maxTurns = maxBoard tl turns ((turns, winNum), bn)
  | otherwise  = maxBoard tl maxTurns b
calcScore :: [(Maybe (Int, Int), Board)] -> Int
calcScore [] = 0
calcScore maybeWn =
  let
    -- covariants..?? interesting.. Also, fromJust is not safe but whatever I guess.
    wns = map (first fromJust ) $  filter (\(a, _) -> isJust a ) maybeWn
    headwns = head wns
    initWn =  fst $ fst $ head wns
    b = maxBoard (tail wns) initWn headwns
    sum = sumUnmarked $ snd b
    winningNumber = snd $ fst b
  in
    sum * winningNumber


main :: IO ()
main = do
  name <- getArgs
  input <- TO.readFile $ head name
  let a = T.lines input
  let b = parseInput "" a []
  -- I don't need to convert to int since I only need equality to compare tbh
  let bingoNumbers = map (read . T.unpack :: T.Text -> Int) $ T.splitOn "," $ head $ head b
  let boardswins = map (flip boardWinsIn bingoNumbers . parseBoard) $ tail b
  -- print boardswins
  print bingoNumbers
  let m = calcScore boardswins
  -- let bingoBoardStrings = tail b
  -- print b
  print m
