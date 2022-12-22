module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

type List = [String]
type Pair = (List, List)

parseInput :: [String] -> [Pair]
parseInput xs = map parsePair $ splitWhen (\x -> x == "") xs

parsePair :: [String] -> Pair
parsePair (x0:x1:xs) = (parseList x0, parseList x1)

parseList :: String -> [String]
parseList str = filter (\x -> x /= ",") $ split (dropInitBlank $ dropInnerBlanks $ dropFinalBlank $ oneOf "[,]") str

readInt :: String -> Int
readInt = read

isCorrect :: Pair -> Int
isCorrect ("[":xs, "[":ys) = isCorrect (xs, ys)
isCorrect ("]":xs, "]":ys) = isCorrect (xs, ys)
isCorrect ("[":xs, "]":ys) = 0
isCorrect ("]":xs, "[":ys) = 1
isCorrect ("[":xs, y:ys) = isCorrect (xs, y:"]":ys)
isCorrect ("]":xs, y:ys) = 1
isCorrect (x:xs, "[":ys) = isCorrect (x:"]":xs, ys)
isCorrect (x:xs, "]":ys) = 0
isCorrect (x:xs, y:ys) = let a = readInt x; b = readInt y
                         in if a < b 
                            then 1 
                            else if a > b then 0 else isCorrect (xs, ys)

part1 :: [Pair] -> Int
part1 xs = sum $ zipWith (\x y -> if y == 1 then x else 0) [1..] $ map isCorrect xs

divider1 = ["[", "[", "2", "]", "]"]
divider2 = ["[", "[", "6", "]", "]"]

part2 :: [Pair] -> Int
part2 xs = let ys = divider1:divider2:(concat $ map (\(x, y) -> [x, y]) xs)
               zs = map concat $ sortBy (\ a b -> if isCorrect (a, b) == 1 then LT else GT) ys
           in foldl (*) 1 $ zipWith (\ x y -> if y == "[[2]]" || y == "[[6]]" then x else 1) [1..] zs

