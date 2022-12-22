module Main where

import Data.Char
import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . lines $ contents

part1 :: [String] -> Int
part1 x = (sum . concat . map findCommon . map splitHalf . map strToPrios) x

part2 :: [String] -> Int
part2 x = (sum . concat . map findCommon . chunksOf 3 . map strToPrios) x

strToPrios :: String -> [Int]
strToPrios s = map prio s

prio :: Char -> Int
prio c = let x = ord c in if x < ord 'a' then x - ord 'A' + 27 else x - ord 'a' + 1

splitHalf :: [Int] -> [[Int]]
splitHalf xs = let h = length xs `div` 2 in [take h xs, drop h xs]

findCommon :: [[Int]] -> [Int]
findCommon xs = nub (foldr intersect (head xs) (tail xs))

