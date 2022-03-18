-- |

module Main where
import qualified Text.Parsec as P
import System.Environment (getArgs)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser, GenTokenParser (integer, commaSep, symbol))
import Text.Parsec (unexpected,  many,  parse, endOfLine, sepBy1)
import Control.Monad (void)
import Data.List (foldl')
import Data.Map.Strict (Map, empty, insertWith, elems)
import Debug.Trace (trace)

-- represents a line
data Point = Point {
  x :: Integer,
  y :: Integer
                   } deriving ( Eq, Ord)
instance Show Point where
  show (Point x1 y1) = mconcat ["<", show x1, "," , show y1, ">"]
data Line = Line {
  start :: Point,
  end :: Point
                 }
instance Show Line where
  show (Line (Point x1 y1) (Point x2 y2)) = mconcat  [ show x1, ",", show y1, "->" , show x2, ",", show y2]
data LineClass = Vertical | Horizontal | DiagonalUp | DiagonalDown deriving Show
-- Sparse matrix? that maps the points to how many times they overlap
-- Probably the easiest solution (even if it's not as much space efficient)
type Grid = Map Point Integer

lexer = makeTokenParser emptyDef
int = integer lexer
csep = commaSep lexer
sym = symbol lexer

parsePoint :: Parser Point
parsePoint = do
  ints <- csep int
  case ints of
    [a,b] -> return $ Point a b
    _ -> unexpected "Expected a list of 2 ints"
-- Parse a string into data
parseLine :: Parser Line
parseLine = do
  start <- parsePoint
  sym "->"
  end <- parsePoint
  -- char '\n'
  return (Line start end)

parseData :: Parser [ Line ]
parseData =  many parseLine
-- keep some kind of sparse matrix maybe?? Or a dictionary? Anyways, you need to store the lines in a data structure such that you can get the overlap information
-- Other methods considered:
-- 3 cases, the line can overlap vertically, horizontally, or orthongonally.
-- veritically: lines have same y coordinate, then overlap is absolute value. ( (xstart1 + xend1) - (xstart2 + xend2) ). Similar for horizontal overlap.
-- Orthongonal overlap is harder to determine actually (that's why the dictionary method doesn't work).... This is probably why it's better to mark the points in the grid.

classifyLine :: Line -> LineClass
classifyLine (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = Vertical
  | y1 == y2 = Horizontal
  -- cases for diagonal lines
  | (x2 > x1) && (y2 > y1) = DiagonalUp
  | (x2 > x1) && (y2 < y1) = DiagonalDown
  | (x1 > x2) && (y1 > y2) = DiagonalUp
  | (x1 > x2) && (y1 < y2) = DiagonalDown
  -- other cases are not mathced
  | otherwise = DiagonalDown
-- returns 0 if line isn't diagonal
calcB :: Line -> Integer
calcB line =
  case classifyLine line of
    DiagonalUp ->
      y (start line) - x (start line)
    DiagonalDown ->
      -(x (start line) + y (start line))
    _ -> 0
markLine ::   Grid -> Line   -> Grid
markLine grid line =
  case classifyLine line of
    -- the list comprehension are doulbed because something like [9..4] yields [] .. and start and end can be in any order so this was the "easiest" solution.
    Horizontal ->  foldl' (\map i -> insertWith (+) (Point  i (y $ end line)) 1 map) grid ([( x $ start line) .. (x $ end line)] ++ [( x $ end line) .. (x $ start line)])
    Vertical -> foldl' (\map i -> insertWith (+) (Point  (x $ end line) i ) 1 map) grid ([( y $ start line) .. (y $ end line)] ++ [( y $ end line) .. (y $ start line)])
    -- disgusting code.. I guess I can fix this by abstracting this pattern into a function?? Whatever I guess lols
    DiagonalUp -> foldl' (\map (a,b) -> insertWith (+) (Point  a b) 1 map)  grid  $ zip  ([( x $ start line) .. (x $ end line)] ++ [( x $ end line) .. (x $ start line)]) ([( y $ start line) .. (y $ end line)] ++ [( y $ end line) .. (y $ start line)])
    DiagonalDown -> foldl' (\map (a,b) -> insertWith (+) (Point  a b) 1 map)  grid  $ zip  ([( x $ start line) .. (x $ end line)] ++ [( x $ end line) .. (x $ start line)]) (reverse ([( y $ start line) .. (y $ end line)] ++ [( y $ end line) .. (y $ start line)]) )


-- sorted array of vertical/horizontal lines? I can always get the length of the input before hand maybe..
main :: IO ()
main = do
  name <- getArgs
  input <- readFile $ head name
  -- let a = map .unpack $ T.lines input
  let lines =   Text.Parsec.parse parseData "" input
  case lines of
    Left pe -> error $ show pe
    Right lis -> do
      let grid = foldl' markLine empty lis :: Grid
      -- print lis
      let ptsGtr2 = length $ filter (> 1) $  elems grid
      print ptsGtr2
      -- print grid

  -- putStrLn  input
