module Main where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [((Int,Int),(Int,Int))]
parseInput xs = map (parseCoords . splitOneOf "=,:") xs

parseCoords :: [String] -> ((Int,Int),(Int,Int))
parseCoords xs = ((read (xs !! 1),read (xs !! 3)),(read (xs !! 5),read (xs !! 7)))

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

pointsOn :: Int -> ((Int,Int),(Int,Int)) -> Maybe (Int,Int)
pointsOn y ((x0, y0),xy1) = let d = manhattan (x0, y0) xy1;
                                dy = abs (y0 - y)
                            in if dy <= d 
                               then Just (x0 - (d - dy), x0 + (d - dy))
                               else Nothing

unionRanges :: [(Int,Int)] -> (Int, Int) -> [(Int, Int)]
unionRanges [] r = [r]
unionRanges ((x0,x1):xs) (r0, r1) = if x1 < r0
                                    then (r0, r1):(x0, x1):xs
                                    else (x0, maximum [x1, r1]):xs

rangeLength :: (Int, Int) -> Int
rangeLength (x0, x1) = x1 - x0 + 1

inRange :: [(Int, Int)] -> Int -> Int
inRange [] x = 0
inRange ((x0, x1):xs) x = if x0 <= x && x <= x1 then 1 else inRange xs x 

yl = 2000000

part1 :: [((Int,Int),(Int,Int))] -> Int
part1 xs = let bs = nub $ mapMaybe (\(_,(x,y)) -> if y == yl then Just x else Nothing) xs;
               rs = foldl unionRanges [] $ sort $ mapMaybe (pointsOn yl) xs
           in (sum $ map rangeLength rs) - (sum $ map (inRange rs) bs)

removeRanges :: (Int, Int) -> [(Int,Int)] -> [(Int, Int)]
removeRanges r [] = [r]
removeRanges (r0, r1) ((x0,x1):xs) = if x1 < r0 || r1 < x0
                                     then removeRanges (r0, r1) xs
                                     else if x0 <= r0 && r1 <= x1
                                          then []
                                          else if r0 < x0 && x1 < r1
                                               then (removeRanges (r0, x0 - 1) xs) ++ (removeRanges (x1 + 1, r1) xs)
                                               else if r0 < x0 && x0 <= r1
                                                    then removeRanges (r0, x0 - 1) xs
                                                    else removeRanges (x1 + 1, r1) xs

points :: [((Int,Int),(Int,Int))] -> Int -> [(Int, Int)]
points xs y = let rs = foldl unionRanges [] $ sort $ mapMaybe (pointsOn y) xs
              in removeRanges (0, yu) rs

yu = 4000000

part2 :: [((Int,Int),(Int,Int))] -> Int
part2 xs = let (a,b) = head $ mapMaybe (\y -> let ps = points xs y in if ps == [] then Nothing else Just (fst (head ps), y)) [0..yu]
           in a * yu + b
