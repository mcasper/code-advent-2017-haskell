#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils

data Leaf = Leaf { name :: String
                 , weight :: Int
                 , leaves :: [Leaf] } deriving (Show)

instance Eq Leaf where
  (==) left right = (name left) == (name right)

main = do
  input <- readFile "inputs/day_seven"
  let leaves = map parseLeaf (splitOn "\n" (rstrip input))
  let leaf = collapseTree leaves
  putStrLn (show leaf)
  -- putStrLn (weights leaf)

collapseTree :: [Leaf] -> Leaf
collapseTree leaves
  | (length leaves) == 1 = leaves !! 0
  | otherwise            = do
    let leaf = head leaves
    let rest = tail leaves
    let maybeParentIndex = findIndex (\x -> containsLeaf leaf x) rest
    collapseTree (newTree leaf rest maybeParentIndex)

newTree :: Leaf -> [Leaf] -> Maybe Int -> [Leaf]
newTree leaf leaves Nothing = leaves ++ [leaf]
newTree leaf leaves (Just index) = do
  let newLeaf = insertLeaf leaf (leaves !! index)
  let (front, rest) = splitAt index leaves
  front ++ (newLeaf : (tail rest))

insertLeaf :: Leaf -> Leaf -> Leaf
insertLeaf child leaf
  | elem child (leaves leaf) = do
    let index = fromJust (elemIndex child (leaves leaf))
    let (front, rest) = splitAt index (leaves leaf)
    let newLeaves = front ++ (child : (drop 1 (leaves leaf)))
    Leaf {name=(name leaf), weight=(weight leaf), leaves=newLeaves}
  | otherwise                = Leaf {name=(name leaf), weight=(weight leaf), leaves=(map (insertLeaf child) (leaves leaf))}

-- combineChildren :: Leaf -> Leaf -> Leaf
-- combineChildren left right = do
--   let newLeaves = map (leaves left) 
--   let weight = maximum [(weight left), (weight right)]
--   Leaf {name=(name left), weight=(weight), leaves=newLeaves}

containsLeaf :: Leaf -> Leaf -> Bool
containsLeaf left right = do
  elem left (leaves right) || any (\x -> containsLeaf left x) (leaves right)

parseLeaf :: String -> Leaf
parseLeaf line = do
  let pieces = splitOn " " line
  let weight = read (take (length (pieces !! 1) - 2) (tail (pieces !! 1)))
  Leaf {name=(pieces !! 0), weight=weight, leaves=(map leafFromName (drop 3 pieces)) }

leafFromName :: String -> Leaf
leafFromName name = Leaf {name=(replace "," "" name), weight=0, leaves=[]}
