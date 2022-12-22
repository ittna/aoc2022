module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

type Blueprint = (Int, Int, Int, Int, Int, Int)

parseInput :: [String] -> [Blueprint]
parseInput xs = map (parseBlueprint . splitOn " ") xs

parseBlueprint :: [String] -> Blueprint
parseBlueprint xs = (read (xs !! 6), read (xs !! 12), read (xs !! 18), read (xs !! 21), read (xs !! 27), read (xs !! 30))

oreCost (x,_,_,_,_,_) = x
claCost (_,x,_,_,_,_) = x
obsCost1 (_,_,x,_,_,_) = x
obsCost2 (_,_,_,x,_,_) = x
geoCost1 (_,_,_,_,x,_) = x
geoCost2 (_,_,_,_,_,x) = x

simulate :: Int -> ((Int, Int, Int, Int), (Int, Int, Int, Int)) -> Blueprint -> Int
simulate 0 (_,(_,_,_,g)) _ = g
simulate m ((nor, ncl, nob, nge), (ror, rcl, rob, rge)) b = maximum $ concat [if ror >= geoCost1 b && rob >= geoCost2 b then [simulate (m - 1) ((nor, ncl, nob, nge + 1), (ror + nor - geoCost1 b, rcl + ncl, rob + nob - geoCost2 b, rge + nge)) b] else if ror >= obsCost1 b && rcl >= obsCost2 b then [simulate (m - 1) ((nor, ncl, nob + 1, nge), (ror + nor - obsCost1 b, rcl + ncl - obsCost2 b, rob + nob, rge + nge)) b] else [if ror >= claCost b then simulate (m - 1) ((nor, ncl + 1, nob, nge), (ror + nor - claCost b, rcl + ncl, rob + nob, rge + nge)) b else 0, if ror >= oreCost b then simulate (m - 1) ((nor + 1, ncl, nob, nge), (ror + nor - oreCost b, rcl + ncl, rob + nob, rge + nge)) b else simulate (m - 1) ((nor, ncl, nob, nge), (ror + nor, rcl + ncl, rob + nob, rge + nge)) b]]

part1 :: [Blueprint] -> Int
part1 xs = sum $ zipWith (*) [1..] $ map (simulate 24 ((1,0,0,0),(0,0,0,0))) xs

part2 :: [Blueprint] -> [Int]
part2 xs = map (simulate 32 ((1,0,0,0),(0,0,0,0))) $ take 3 xs

