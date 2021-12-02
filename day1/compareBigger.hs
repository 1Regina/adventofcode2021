import Data.List
import System.IO
import Control.Monad


-- 1. read the file
-- 2. convert the IO String into String n integer
-- 3. compare whether next number is bigger
-- 3a. if bigger -> another list
-- 3b. if not bigger -> drop
-- 3c. if same -> drop
-- 4. Count len in the reduced list

main :: IO ()
main = do
  inputIO <- readFile "input.txt"
  let toInt = [ read i :: Integer | i <- words inputIO]
  let result = larger toInt 0

--   let resultAlso = findLarger toInt
  print "day 1 result count where next depth is incremental"
  print result

  print "day 1 part 2"
  let result2 = larger (windows toInt []) 0
  print result2


-- PART 1
larger (x : y : xs) n | y > x = larger (y: xs) n + 1
larger (x : y : xs) n  = larger (y : xs) n
larger _ n = n


-- PART 2
windows (x:y:z:xs) size = windows (y:z:xs) $ size ++ [x+y+z]
windows _ size = size

-- reducedList = []
-- findLarger  (x : y :  xs) | y > x  = reducedList ++ y
--                           | otherwise = reducedList
-- findLarger _ = []