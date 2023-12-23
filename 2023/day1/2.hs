{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import qualified Data.Text as T
import Data.Maybe
import Data.List

spell = zip [1.. ] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

first string
  | isDigit (T.head string) = digitToInt (T.head string)
  | Just x <- find (\(ind, x) -> x `T.isPrefixOf` string) spell = fst x
  | otherwise = first (T.tail string)

final string
  | isDigit (T.head string) = digitToInt (T.head string)
  | Just x <- find (\(ind, x) -> T.reverse x `T.isPrefixOf` string) spell = fst x
  | otherwise = final (T.tail string)


getResult string = 10 * first (T.pack string) + final (T.reverse (T.pack string))

main = do
  str <- getContents  
  print (sum(map getResult (lines str)))
