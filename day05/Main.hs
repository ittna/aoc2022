module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> ([[String]], [(Int, Int, Int)])
parseInput xs = let ys = splitWhen (\x -> x == "") xs; 
                    stacks = ys !! 0;
                    ops = ys !! 1 
                in (parseStacks stacks, parseOps ops)

parseStacks :: [String] -> [[String]]
parseStacks = map (reverse . splitOn " ")

parseOps :: [String] -> [(Int, Int, Int)]
parseOps = map (parseOp . splitOn " ")

parseOp :: [String] -> (Int, Int, Int)
parseOp xs = (read (xs !! 1), read (xs !! 3), read (xs !! 5)) 

applyOp :: ([String] -> [String]) -> [[String]] -> (Int, Int, Int) -> [[String]]
applyOp fn stacks (move, from, to) = 
  let fromStack = stacks !! (from - 1)
  in zipWith (\ idx xs -> if idx == from 
                          then drop move xs
                          else (if idx == to
                                then (fn $ take move fromStack) ++ xs
                                else xs)) 
             [1..] 
             stacks

part1 :: ([[String]], [(Int, Int, Int)]) -> String
part1 (stacks, ops) = concat $ map head $ foldl (applyOp reverse) stacks ops

part2 :: ([[String]], [(Int, Int, Int)]) -> String
part2 (stacks, ops) = concat $ map head $ foldl (applyOp id) stacks ops

