module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        putStrLn . part2 . parseInput . lines $ contents

parseInput :: [String] -> [(Int, Int)]
parseInput xs = map parseCommand xs

parseCommand :: String -> (Int, Int)
parseCommand x = let xs = splitOn " " x
                 in if head xs == "noop"
                    then (1, 0)
                    else (2, read (xs !! 1)) 

execute :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
execute xs (a, b) = let (a0, b0) = head xs
                    in (a0 + a, b0 + b):xs

valueAt :: Int -> [(Int, Int)] -> Int
valueAt x xs = let (_, r) = last $ takeWhile (\ (c, _) -> c < x) xs
                in r

part1 :: [(Int, Int)] -> Int
part1 xs = let ys = reverse $ foldl execute [(0, 1)] xs
           in sum $ map (\x -> x * valueAt x ys) [20,60..220]

part2 :: [(Int, Int)] -> String
part2 xs = let ys = reverse $ foldl execute [(0, 1)] xs
           in unlines $ chunksOf 40 $ map (\x -> let s = 1 + valueAt (x + 1) ys; c = 1 + x `mod` 40 in if abs (s - c) <= 1 then '#' else '.') [0..239]

