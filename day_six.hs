#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils

main = do
  input <- readFile "inputs/day_six"
  let banks = map (read::String->Int) (splitOn "\n" (rstrip input))
  let configurations = cyclesToLoop banks []
  putStrLn ("Part 1: " ++ show (length configurations))
  putStrLn ("Part 2: " ++ show (loopDistance configurations))

cyclesToLoop :: [Int] -> [[Int]] -> [[Int]]
cyclesToLoop banks seen
  | (length (elemIndices banks seen)) > 1 = seen
  | otherwise       = do
    let max = maximum banks
    let index = fromJust (elemIndex max banks)
    let (front, rest) = splitAt index banks
    let newBanks = redistribute (front ++ (0 : (tail rest))) (index + 1) max
    cyclesToLoop newBanks (newBanks : seen)

redistribute :: [Int] -> Int -> Int -> [Int]
redistribute banks index 0 = banks
redistribute banks index bank
  | index >= (length banks) = redistribute banks 0 bank
  | otherwise               = do
    let (front, rest) = splitAt index banks
    let newBanks = front ++ (((head rest) + 1) : tail rest)
    redistribute newBanks (index + 1) (bank - 1)

loopDistance :: [[Int]] -> Int
loopDistance configurations = do
  let loopConfiguration = head configurations
  let indices = elemIndices loopConfiguration configurations
  (indices !! 1) - (indices !! 0)
