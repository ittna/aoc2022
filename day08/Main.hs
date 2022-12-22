module Main where

import Data.Char
import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [[Int]]
parseInput xs = map (map parseInt) xs

parseInt :: Char -> Int
parseInt c = ord c - ord '0'

visibleIdx :: Int -> Int -> [Int] -> [Int]
visibleIdx idx val [] = []
visibleIdx idx val (x:xs) = if x == 9 then [idx] else if x > val then idx:visibleIdx (idx + 1) x xs else visibleIdx (idx + 1) val xs   

visible :: [Int] -> [Int]
visible xs = let l = length xs;
                 a = visibleIdx 0 (-1) xs;
                 b = map (\x -> l - x - 1) (visibleIdx 0 (-1) (reverse xs))
             in nub (a ++ b) 

part1 :: [[Int]] -> Int
part1 xs = let ys = transpose xs
               visibleXs = concat $ zipWith (\x y -> zip (visible y) (repeat x)) [0..] xs;
               visibleYs = concat $ zipWith (\x y -> zip (repeat x) (visible y)) [0..] ys
           in length $ nub (visibleXs ++ visibleYs)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

distance :: [Int] -> [Int]
distance [] = []
distance (x:xs) = (length (takeWhileInclusive (\y -> y < x) xs):distance xs)

score :: [Int] -> [Int]
score xs = let fwd = distance xs; rwd = reverse $ distance $ reverse xs
           in zipWith (*) fwd rwd

part2 :: [[Int]] -> Int
part2 xs = let ys = transpose xs;
               scoreXs = map score xs;
               scoreYs = transpose $ map score ys
           in maximum $ concat $ zipWith (\x y -> zipWith (*) x y) scoreXs scoreYs

