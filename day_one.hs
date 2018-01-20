#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
import Data.Char
import Data.String.Utils

main = do
  input <- readFile "inputs/day_one"
  putStrLn (sumRepeatingNums ((last (rstrip (input))) : (rstrip input)) 0)

sumRepeatingNums :: [Char] -> Integer -> String
sumRepeatingNums [] sum = show sum
sumRepeatingNums [a] sum = show sum
sumRepeatingNums chars sum = do
  let [char1, char2] = take 2 chars
  sumRepeatingNums (drop 1 chars) (newSum char1 char2 sum)

newSum :: Char -> Char -> Integer -> Integer
newSum char1 char2 sum
  | char1 == char2 = sum + (toInteger (digitToInt char1))
  | otherwise      = sum
