module Main where

import Data.Char
import Data.Graph.AStar
import Data.List
import Data.List.Split
import Data.Maybe

import qualified Data.HashSet as HS

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [String]
parseInput xs = xs

cellAt :: [String] -> (Int, Int) -> Maybe Char
cellAt xs (x, y) = if y >= 0 && y < length xs
                   then let ys = xs !! y 
                        in if x >= 0 && x < length ys then Just (ys !! x) else Nothing
                   else Nothing

elevation :: Maybe Char -> Maybe Char
elevation (Just 'S') = Just 'a'
elevation (Just 'E') = Just 'z'
elevation c = c

isNavigable :: Maybe Char -> Maybe Char -> Bool
isNavigable x y = let xs = catMaybes [x, y]
                  in if length xs < 2 then False else ord (xs !! 0) - ord (xs !! 1) <= 1

isNeighbour :: [String] -> Maybe Char -> (Int, Int) -> Maybe (Int, Int)
isNeighbour xs c (x, y) = let c1 = elevation $ cellAt xs (x, y)
                          in if isNavigable c c1 then Just (x, y) else Nothing 

neighbours :: [String] -> (Int, Int) -> HS.HashSet (Int, Int)
neighbours xs (x, y) = let c = elevation $ cellAt xs (x, y);
                           adj = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                       in HS.fromList $ mapMaybe (isNeighbour xs c) adj

part1 :: [String] -> Int
part1 xs = let start = (0, 20);
               (endX, endY) = (138, 20);
               path = aStar (neighbours xs)
                            (\_ _ -> 1)
                            (\(x, y) -> abs (endX - x) + abs (endY - y))
                            (\xy -> cellAt xs xy == Just 'E')
                            start
           in length $ fromMaybe [] path

part2 :: [String] -> Int
part2 xs = let start = (138, 20);
               path = aStar (neighbours xs)
                            (\_ _ -> 1)
                            (\_ -> 1)
                            (\xy -> cellAt xs xy == Just 'a')
                            start
           in length $ fromMaybe [] path

