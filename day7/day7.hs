-- |

module Main where
import qualified Data.Text.IO as TO
import qualified Data.Text as T
import System.Environment (getArgs)
-- given a list of numbers [a], minimize sum_a (a - target)

-- will the "target" be in the list of numbers [a] ?? I seems that it at least has to be in the range of a
-- brute force solution .. still kind of fast tho I guess. Tbh, I cheated (once again) by referring to this guy: https://www.youtube.com/watch?v=I_GB8DMGvVA

-- actually, I wonder if it's still valid if the targets were limited to the ones inthe list
-- from looking at other people's solutions, it seems that the "optimal" solution is about using derivatives, which involves calculus and stuff...
-- and that'll allow you to do a binary search(usign the derivatives) to find the right solution??
-- If a derivative go from decreasing to increasing, then you found the minimum
check :: Int -> [Int] -> Int
check target = foldr (\a b -> b + abs (a - target)) 0
-- optimal haskell solution here: https://www.youtube.com/watch?v=OZF5C9dHlC8
-- I don't want tc copy it here and claim it as mine tbh, so I'll leave my solution as unoptimal. I wish I didn't give up so fast but math scares me..
-- Anyways, it's basically the newtonain method + binary search used to find the minimum


-- calculate the fuel cost for part 2
check2 :: Int -> [Int] -> Int
check2 target = foldr (\a b -> b + factSum a) 0
  where
    val a = abs (a - target)
    -- not sure who call this function, but this is a "factorial" with + rather than "*"
    factSum a = sum [1.. (val a)]


main :: IO ()
main = do
  name <- getArgs
  input <- TO.readFile $ head name
  let list = (map (read . T.unpack) $ T.split (==',') $ head $ T.lines input) :: [Int]
  let m = maximum list
  let mi = minimum list
  -- I wonder if you can optimize with a sort of binary search
  let targets =  [mi .. m]
  -- print list
  let minFuel = minimum $ map (`check2` list) targets
  print minFuel
  -- print list
