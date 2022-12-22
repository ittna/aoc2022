module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . parseInput . lines $ contents

parseInput :: [String] -> [[Int]]
parseInput xs = map (map read . splitOn ",") xs

groupByIndex :: Int -> [[Int]] -> [[[Int]]]
groupByIndex i xs = let as = sortBy (\ys zs -> compare (ys !! i) (zs !! i)) xs 
                    in groupBy (\ys zs -> (ys !! i) == (zs !! i)) as

makePairIndex :: Int -> [Int] -> (Int, Int)
makePairIndex 0 (_:y:z:[]) = (y, z)  
makePairIndex 1 (x:_:z:[]) = (x, z)  
makePairIndex 2 (x:y:_:[]) = (x, y)  

pairwiseMap :: ([a] -> [a] -> b) -> [[a]] -> [b]
pairwiseMap f (x:[]) = [(f x [])]
pairwiseMap f (x0:x1:xs) = (f x0 x1):(pairwiseMap f (x1:xs))

part1 :: [[Int]] -> Int
part1 xs = sum $ concatMap (\i -> pairwiseMap (\as bs -> length $ (as \\ bs) `union` (bs \\ as)) $ (\ys -> []:(map (map (makePairIndex i)) ys)) $ groupByIndex i xs) [0..2]

convert :: [[Int]] -> [([Int], [Int])]
convert xs = map (\(y:ys) -> let xy = take 2 y; zs = map last (y:ys) in (xy, [(minimum zs)..(maximum zs)])) $ groupBy (\(x0:y0:ys) (x1:y1:zs) -> (x0,y0) == (x1,y1)) $ sort xs

part2 :: [[Int]] -> Int
part2 xs = let x1s = concatMap (\((y:z:ys), zs) -> map (\x -> [x, y, z]) zs) $ convert $ map (\(x:y:z:ys) -> [y, z, x]) xs;
               y1s = concatMap (\((x:z:ys), zs) -> map (\y -> [x, y, z]) zs) $ convert $ map (\(x:y:z:ys) -> [x, z, y]) xs;
               z1s = concatMap (\((x:y:ys), zs) -> map (\z -> [x, y, z]) zs) $ convert xs;
               fs = x1s `intersect` y1s `intersect` z1s
           in sum $ concatMap (\i -> pairwiseMap (\as bs -> length $ (as \\ bs) `union` (bs \\ as)) $ (\ys -> []:(map (map (makePairIndex i)) ys)) $ groupByIndex i fs) [0..2]

