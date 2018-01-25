#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils

data Variable = Variable { identifier :: String
                         , value :: Integer } deriving (Show)

instance Eq Variable where
  (==) left right = (identifier left) == (identifier right)

instance Ord Variable where
  (Variable _ v1) `compare` (Variable _ v2) = v1 `compare` v2

main = do
  input <- readFile "inputs/day_eight"
  -- input <- readFile "inputs/day_eight_test"
  let variables = parseVariables (splitOn "\n" (rstrip input))
  let (newVariables, absoluteMax) = executeInstructions (splitOn "\n" (rstrip input)) variables 0
  putStrLn (show (maximum newVariables))
  putStrLn (show absoluteMax)

executeInstructions :: [String] -> [Variable] -> Integer -> ([Variable], Integer)
executeInstructions lines variables absoluteMax
  | (length lines) == 0 = (variables, absoluteMax)
  | otherwise           = do
    let (newVariables, newAbsoluteMax) = executeInstruction (head lines) variables absoluteMax
    executeInstructions (tail lines) newVariables newAbsoluteMax

executeInstruction :: String -> [Variable] -> Integer -> ([Variable], Integer)
executeInstruction line variables absoluteMax = do
  let pieces = splitOn " " line
  let [var, direction, val, wut, condVar, cond, condVal] = splitOn " " line
  let foundVar = fromJust (find (\x -> (identifier x) == var) variables)
  let foundCondVar = fromJust (find (\x -> (identifier x) == condVar) variables)
  if (condition foundCondVar cond (read condVal)) then do
                                                    let index = fromJust (elemIndex foundVar variables)
                                                    let newVar = incOrDec foundVar direction (read val)
                                                    let newVars = replaceNth index newVar variables
                                                    (newVars, (maximum [(value newVar), absoluteMax]))
  else (variables, absoluteMax)

incOrDec :: Variable -> String -> Integer -> Variable
incOrDec variable "inc" val = Variable{identifier=(identifier variable), value=((value variable) + val)}
incOrDec variable "dec" val = Variable{identifier=(identifier variable), value=((value variable) - val)}

condition :: Variable -> String -> Integer -> Bool
condition variable "!=" val = (value variable) /= val
condition variable "==" val = (value variable) == val
condition variable ">" val = (value variable) > val
condition variable "<" val = (value variable) < val
condition variable ">=" val = (value variable) >= val
condition variable "<=" val = (value variable) <= val
condition variable operator val = error ("Unknown operator " ++ operator)

parseVariables :: [String] -> [Variable]
parseVariables lines = map parseVariable lines

parseVariable :: String -> Variable
parseVariable line = Variable {identifier=((splitOn " " line) !! 0), value=0}

replaceNth :: (Show a) => Int -> a -> [a] -> [a]
replaceNth index value values = do
  let (front, rest) = splitAt index values
  front ++ (value : (tail rest))
