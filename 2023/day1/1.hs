-- | day1 aoc

module Day1 where
import Data.Char

getResult string =
  let nums = [x | x <- string, isDigit x] in
    10 * digitToInt (head nums) + digitToInt (last nums)

main = do
  str <- getContents  
  print (sum(map getResult (lines str)))          
          



