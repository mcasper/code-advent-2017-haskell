#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
import Data.Char
import Data.String.Utils
import Data.List.Split

main = do
  input <- readFile "inputs/day_five"
  let nums = map (read::String->Int) (splitOn "\n" (rstrip input))
  putStrLn ("Part 1: " ++ show (escapeMaze nums (\x -> x + 1) 0 0))
  putStrLn ("Part 2: " ++ show (escapeMaze nums incrOrDecr 0 0))

escapeMaze :: [Int] -> (Int -> Int) -> Int -> Int -> Int
escapeMaze jumps incrementer index moves
  | index >= (length jumps) = moves
  | otherwise                     = do
    let (front, rest) = splitAt index jumps
    let instruction = head rest
    let newIndex = index + instruction
    let newInstruction = incrementer instruction
    escapeMaze (front ++ (newInstruction : (tail rest))) incrementer newIndex (moves + 1)

incrOrDecr :: Int -> Int
incrOrDecr instruction
  | instruction > 2 = instruction - 1
  | otherwise       = instruction + 1
