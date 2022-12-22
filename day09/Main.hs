module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . words $ contents

parseInput :: [String] -> [(String, Int)]
parseInput xs = pairwise xs

pairwise :: [String] -> [(String,Int)]
pairwise (x:y:xs) = (x, read y):pairwise xs
pairwise [] = []

direction :: String -> (Int, Int)
direction "R" = (1, 0)
direction "L" = (-1, 0)
direction "U" = (0, 1)
direction "D" = (0, -1)

step :: [(Int, Int)] -> String -> [(Int, Int)]
step xs d = let (dx, dy) = direction d
            in foldl (\ ys (x, y) -> if length ys == 0 
                                     then [(x + dx, y + dy)]
                                     else let (xh, yh) = head ys;
                                              (sx, sy) = (signum (xh - x), signum (yh - y));
                                              adj = abs (xh - x) <= 1 && abs (yh - y) <= 1
                                          in (if adj then x else x + sx, if adj then y else y + sy):ys)
                     [] (reverse xs)

move :: [[(Int, Int)]] -> String -> [[(Int, Int)]]
move xs op = let p = head xs in ((step p op):xs)

part1 :: [(String, Int)] -> Int
part1 xs = let ops = reverse $ foldl (\ ys (d, c) -> (take c $ repeat d) ++ ys) [] xs
           in length $ nub $ map head (foldl move [take 2 $ repeat (0, 0)] ops)

part2 :: [(String, Int)] -> Int
part2 xs = let ops = reverse $ foldl (\ ys (d, c) -> (take c $ repeat d) ++ ys) [] xs
           in length $ nub $ map head (foldl move [take 10 $ repeat (0, 0)] ops)

