module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [Char]
parseInput xs = concat xs

unique :: Int -> [Char] -> Bool
unique n cs = (length $ nub $ take n cs) == n

part1 :: [Char] -> Int
part1 cs = if unique 4 cs then 4 else 1 + part1 (tail cs)

part2 :: [Char] -> Int
part2 cs = if unique 14 cs then 14 else 1 + part2 (tail cs)

