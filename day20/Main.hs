module Main where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO()
main = do
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [Int]
parseInput xs = map read xs

arrange :: [(Int, Int)] -> Int -> [(Int, Int)]
arrange xs n = if n == length xs
               then xs
               else let (i, x) = xs !! n;
                        j = (i + x) `mod` (length xs - 1);
                        ys = if i == j then xs else map (\(k, y) -> (go i j k, y)) xs
                    in arrange ys (n + 1)
  where go :: Int -> Int -> Int -> Int
        go i j k = if k == i
                   then j
                   else if i < j
                        then if k < i || k > j then k else k - 1
                        else if k < j || k > i then k else k + 1

part1 :: [Int] -> Int
part1 xs = let ys = zip [0..] xs
               zs = dropWhile (/= 0) $ cycle $ map snd $ sortBy (\(i,_) (j,_) -> compare i j) $ arrange ys 0
           in sum [zs !! 1000, zs !! 2000, zs !! 3000]

part2 :: [Int] -> Int
part2 xs = let ys = zip [0..] $ map (* 811589153) xs
               ys' = foldl arrange ys $ replicate 10 0
               zs = dropWhile (/= 0) $ cycle $ map snd $ sortBy (\(i,_) (j,_) -> compare i j) ys'
           in sum [zs !! 1000, zs !! 2000, zs !! 3000]

