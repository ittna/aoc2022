module Main where

import Data.Graph.AStar
import Data.List
import Data.List.Split
import Data.Maybe

import qualified Data.HashSet as HS

main :: IO()
main = do
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

type Valve = (String, Int, [String])

parseInput :: [String] -> [Valve]
parseInput xs = map (parseValve . split (dropBlanks $ dropDelims $ oneOf " =,;")) xs

parseValve :: [String] -> Valve
parseValve (_:x:_:_:_:y:_:_:_:_:xs) = (x, read y, xs)

valve :: [Valve] -> String -> Valve
valve (x:xs) y = let (a,_,_) = x in if a == y then x else valve xs y

removeValve :: [Valve] -> String -> [Valve]
removeValve [] _ = []
removeValve (x:xs) y = let (a,b,ys) = x;
                           nys = delete y ys
                       in if a == y
                          then removeValve xs y
                          else (a,b,nys):(removeValve xs y)

neighbours :: Valve -> [String]
neighbours (_, _, xs) = xs

flowrate :: Valve -> Int
flowrate (_, x, _) = x

path :: [Valve] -> String -> String -> Maybe (String, Int, Int)
path xs a b = if a == b
              then Nothing
              else let p = aStar (\x -> HS.fromList $ neighbours $ valve xs x)
                                 (\_ _ -> 1)
                                 (\_ -> 1)
                                 (\x -> x == b)
                                 a;
                       fr = flowrate $ valve xs b
                   in maybe Nothing (\x -> Just (b, fr, 1 + length x)) p

findP :: [String] -> [Valve] -> (Int, String, [String], Int) -> Int
findP ps xs (m, y, ys, fr) = let ns = ps `intersect` ((map (\(x,_,_) -> x) $ filter (\(_,x,_) -> x > 0) xs) \\ ys)
                             in if ns == []
                                then if m < 2 then 0 else fr * (m - 1)
                                else let rs = mapMaybe (path xs y) ns
                                     in maximum $ foldl (\zs (n, a, b) -> if m <= b then (fr * (m - 1)):zs else (fr*b + a + (findP ps xs (m - b, n, n:ys, fr + a))):zs) [] rs

part1 :: [Valve] -> Int
part1 xs = let ns = map (\(x,_,_) -> x) $ filter (\(_,x,_) -> x > 0) xs
           in findP ns xs (30, "AA", [], 0)

subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

allSubsetPairs xs = concat $ foldl (\ys n -> (map (\zs -> (zs, xs \\ zs)) $ subsets n xs):ys) [] [1..(length xs `div` 2)]

mapIndexedPairs xs = let ys = [["FC"],["JA"],["LR","KJ","MH"],["SA"],["JF","WC","EW","IG"],["FP"],["SJ"],["VN"],["RL","ZD"]];
                         f = (\x -> ys !! x)
                     in map (\(as,bs) -> (concat $ map f as, concat $ map f bs)) xs

part2 :: [Valve] -> Int
part2 xs = let ns = mapIndexedPairs $ allSubsetPairs [0..8]
           in maximum $ map (\(ns1,ns2) -> (findP ns1 xs (26, "AA", [], 0)) + (findP ns2 xs (26, "AA", [], 0))) ns

draw :: [Valve] -> String
draw xs = unlines $ nub $ sort $ concat $ map (\(a, _, ys) -> map (\b -> intercalate " <--> " (sort (a:[b]))) ys) xs

