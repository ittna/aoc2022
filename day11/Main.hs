module Main where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [Monkey]
parseInput xs = map parseMonkey $ splitWhen (\x -> x == "") xs

type Monkey = ([Int], Int -> Int, Int, Int -> Int, Int)

parseMonkey :: [String] -> Monkey
parseMonkey xs = let items = drop 2 $ split (dropBlanks $ dropDelims $ oneOf " ,") (xs !! 1);
                     op = drop 5 $ splitOn " " (xs !! 2);
                     test = last $ splitOn " " (xs !! 3);
                     true = last $ splitOn " " (xs !! 4);
                     false = last $ splitOn " " (xs !! 5);
                     d = read test
                 in (map read items, parseOp op, d, parseTest (d, read true, read false), 0)

parseOp :: [String] -> (Int -> Int)
parseOp xs x = let op = if (xs !! 1) == "*" then (*) else (+);
                   y = if (xs !! 2) == "old" then Nothing else Just (read (xs !! 2))
               in op x $ fromMaybe x y

parseTest :: (Int, Int, Int) -> (Int -> Int)
parseTest (d, t, f) x = if x `mod` d == 0 then t else f

runSteps :: (Int -> Int) -> [Monkey] -> Int -> [Monkey]
runSteps fn xs idx = let (items, op, _, to, _) = xs !! idx;
                         ys = map (fn . op) items;
                         zs = zip (map to ys) ys
                     in zipWith (\i (items, o, d, t, c) -> if i == idx then ([], o, d, t, c + length items) else let as = map snd $ filter (\(a,_) -> a == i) zs in (items ++ as, o, d, t, c)) [0..] xs

runRound :: (Int -> Int) -> [Monkey] -> [Monkey]
runRound fn xs = foldl (runSteps fn) xs [0..(length xs - 1)]

part1 :: [Monkey] -> Int
part1 xs = foldl (*) 1 $ take 2 $ reverse $ sort $ map (\(_, _, _, _, x) -> x) $ foldl (\ ys _ -> (runRound (\y -> y `div` 3)) ys) xs [1..20]

part2 :: [Monkey] -> Int
part2 xs = let m = foldl (*) 1 $ map (\(_, _, d, _, _) -> d) xs
           in foldl (*) 1 $ take 2 $ reverse $ sort $ map (\(_, _, _, _, x) -> x) $ foldl (\ ys _ -> (runRound (\y -> y `mod` m)) ys) xs [1..10000]

