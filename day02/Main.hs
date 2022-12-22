module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . words $ contents

part1 :: [String] -> Int
part1 x = (sum . map score . pairwise) x

part2 :: [String] -> Int
part2 x = (sum . map score . map play . pairwise) x

play :: (Int, Int) -> (Int, Int)
play (x, y) = (x, ((x + y - 3) `mod` 3) + 1)

pairwise :: [String] -> [(Int,Int)]
pairwise (x:y:xs) = [(move x, move y)] ++ pairwise xs
pairwise [] = []

score :: (Int, Int) -> Int
score (x, y) = -3 * (((x - y + 1) `mod` 3) - 1) + 3 + y

move :: String -> Int
move x
   | x == "A" || x == "X" = 1
   | x == "B" || x == "Y" = 2
   | x == "C" || x == "Z" = 3
   | otherwise            = 0

