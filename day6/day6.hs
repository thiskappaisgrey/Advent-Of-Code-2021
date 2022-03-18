{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module Main where
import qualified Data.Text.IO as TO
import qualified Data.Text as T
import System.Environment (getArgs)
import Data.List (foldl')
import Debug.Trace (trace, traceShow)
import qualified Data.IntMap.Strict as M

newtype Fish = Fish Int deriving (Num, Eq, Show, Ord)
type School = [Fish]


-- trivial but inefficient solution for Part 1
-- progress the fish by one day
-- precondition, the fish's int is positive

-- this is inefficient because it needs to traverse the list a lot probably??
-- This is, I think, O(n) where n is the lenght of the fishes. Since n increases quite fast, this is probably very inefficient
-- because there's a lot of duplicate fish, this is slow. For example, a school might have a bunch of [ Fish 1, Fish 1, ... ] etc.
progressHelper :: School -> School -> School
progressHelper  newfish (fish:rest)
  | fish == 0 = progressHelper (Fish 6 : Fish 8 : newfish) rest
  | otherwise  = progressHelper (fish - 1 : newfish) rest
progressHelper  newfish [] = newfish

progress :: School -> School
progress = progressHelper []

progressMany :: School -> Int -> School
progressMany school days = foldl' (const . progress) school [1..days]

-- How do I make this faster?? Part 2
-- Solution inspired by: Allan Malloy (I watched a bit of the video) https://www.youtube.com/watch?v=IkR1liQhSa4&list=PLKDpSfFMS1VQROyYkjXbI7sO-cU8QeaSS&index=6
-- Instead, store a mapping between the count of the Timer to the count of the fish with that timer (since id doesn't matter).
-- Timer 7 -> 2 would  mean 2 fish have a timer of 7
type FishMap = M.IntMap Int
insertFish :: Num a => M.IntMap a -> Fish -> M.IntMap a
insertFish map (Fish k) = M.insertWith (+) k 1 map

initSchool :: [Fish] -> FishMap
initSchool = foldl' insertFish initMap
  where
    initMap = M.fromList $ zip [0..8] (repeat 0) -- take a list of fish

progressMap :: FishMap -> FishMap
progressMap fm = shuffle ascs M.empty fm
  where
    -- I need to essentially move the lenghts around
    ascs = M.assocs fm
-- This function took me forever for some reason.... lols
-- I think it's because I forgot about separating the newfish from fishmap
shuffle :: [(Int, Int)] -> FishMap -> FishMap -> FishMap
shuffle [] newfish fm = M.unionWith (+) newfish fm
-- if count is 0, then do nothing
shuffle ((b,0):tail) newfish fm = shuffle tail newfish fm
-- add to newfish (which is a separate map so the other functions don't mess with it)
shuffle ((0,a):tail) newfish fm = shuffle tail (M.insertWith (+) 8 a $ M.insertWith (+) 6 a newfish) (M.insert  0 0 fm)
-- shuffle the fish count around by swapping their counts
shuffle ((day,count):tail) newfish fm = shuffle tail newfish (M.insert (day-1) count $ M.insert day 0 fm)

progresMapMany :: FishMap -> Int -> FishMap
progresMapMany fm days = foldl' (const . progressMap) fm [1..days]

getFish :: FishMap -> Int
getFish = M.foldr (+) 0


main :: IO ()
main = do
  name <- getArgs
  input <- TO.readFile $ head name
  let school = map (Fish . read . T.unpack) $ T.split (==',') $ head $ T.lines input
  -- print school
  print $ getFish $ progresMapMany (initSchool school) 256
