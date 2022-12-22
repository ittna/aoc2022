module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . lines $ contents

part1 :: [String] -> Int
part1 x = (sum . map (score . map read . splitOneOf ",-")) x

part2 :: [String] -> Int
part2 x = (sum . map (scoreOverlap . map read . splitOneOf ",-")) x 

score :: [Int] -> Int
score x = let a0 = x !! 0; a1 = x !! 1; b0 = x !! 2; b1 = x !! 3 
           in if (a0 <= b0 && a1 >= b1) || (a0 >= b0 && a1 <= b1) then 1 else 0

scoreOverlap :: [Int] -> Int
scoreOverlap x = let a0 = x !! 0; a1 = x !! 1; b0 = x !! 2; b1 = x !! 3 
                  in if (a0 < b0 && a1 < b0) || (a0 > b1 && a1 > b1) then 0 else 1


