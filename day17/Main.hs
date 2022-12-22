module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        putStr . part1 . parseInput . lines $ contents

parseInput :: [String] -> [Int]
parseInput xs = map (\c -> if c == '<' then -1 else 1) $ concat xs

e = "|.......|"
r1 = ["|..@@@@.|"]
r2 = ["|...@...|", "|..@@@..|", "|...@...|"]
r3 = ["|....@..|", "|....@..|", "|..@@@..|"]
r4 = ["|..@....|", "|..@....|", "|..@....|", "|..@....|"]
r5 = ["|..@@...|", "|..@@...|"]

rocks = cycle [r1, r2, r3, r4, r5]

isBlocked :: String -> Bool
isBlocked (_:[]) = False
isBlocked (x0:x1:xs) = if x0 == '@' && (x1 `elem` "#-|")
                       then True
                       else isBlocked (x1:xs)

froze :: String -> String
froze "" = ""
froze (x:xs) = (if x == '@' then '#' else x):(froze xs)

move :: Int -> String -> String
move _ (x:[]) = [x]
move 1 xs = reverse $ move (-1) $ reverse xs
move (-1) (x0:x1:xs) = if x1 == '@' then x1:(move (-1) (x0:xs)) else x0:(move (-1) (x1:xs))

fall :: [String] -> [String]
fall xs = let ys = transpose xs
          in if any isBlocked ys
             then transpose $ map froze ys
             else transpose $ map (move 1) ys

step :: Int -> [String] -> [String]
step 1 xs = if any isBlocked xs
            then xs
            else map (move 1) xs
step (-1) xs = if any (isBlocked . reverse) xs
               then xs
               else map (move (-1)) xs

sim :: [String] -> [Int] -> ([String], [Int])
sim xs (y:ys) = if all (/= '@') $ concat xs
                then (xs, y:ys)
                else sim (fall $ step y xs) ys

simulate :: [[String]] -> [Int] -> [String] -> [String]
simulate [] _ xs = xs
simulate (x:xs) ys zs = let rs = (replicate 3 e) ++ zs;
                            (nzs, nys) = sim (x ++ rs) ys;
                            czs = dropWhile (== e) nzs
                        in simulate xs nys czs

part1 :: [Int] -> String
part1 xs = unlines $ simulate (take 2022 rocks) (cycle xs) ["+-------+"]

-- 422

part2 :: [Int] -> String
part2 xs = let ps = simulate (take (422 + 1720) rocks) (cycle xs) ["+-------+"]
           in unlines ps

