module Main where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

data Move = F Int | R | L deriving Show
data Dir = DirU | DirR | DirD | DirL deriving Show 

parseInput :: [String] -> ([Move], [String])
parseInput xs = let (ys, zs) = (last xs, init xs)
                in (parseMoves ys, normalizeMap $ init zs)

parseMoves :: String -> [Move]
parseMoves xs = map parseMove $ split (oneOf "LR") xs

parseMove :: String -> Move
parseMove "R" = R
parseMove "L" = L
parseMove x = F $ read x

normalizeMap :: [String] -> [String]
normalizeMap xs = let l = maximum $ map length xs;
                      ys = map (\ys -> if l > length ys then ys ++ (replicate (l - length ys) ' ') else ys) xs;
                      zs = [replicate l ' '] ++ ys ++ [replicate l ' ']
                  in transpose $ [replicate (length zs) ' '] ++ (transpose zs) ++ [replicate (length zs) ' ']

findStart :: [String] -> (Int, Int, Dir)
findStart (_:x:xs) = let i = fromJust $ '.' `elemIndex` x
                   in (i, 1, DirR)

turn :: Move -> Dir -> Dir
turn R DirU = DirR
turn L DirU = DirL 
turn R DirR = DirD
turn L DirR = DirU 
turn R DirD = DirL
turn L DirD = DirR 
turn R DirL = DirU
turn L DirL = DirD 

dx :: Dir -> Int
dx DirU = 0
dx DirR = 1
dx DirD = 0
dx DirL = (-1)
dy :: Dir -> Int
dy DirU = (-1)
dy DirR = 0
dy DirD = 1
dy DirL = 0

nextCoordInDir :: ((Int, Int, Dir) -> (Int, Int, Dir)) -> [String] -> (Int, Int, Dir) -> Maybe (Int, Int, Dir)
nextCoordInDir f xs (x0, y0, d) = let w = length $ head xs;
                                      h = length xs;
                                      (x, y) = ((x0 + dx d) `mod` w, (y0 + dy d) `mod` h);
                                  in case ((xs !! y) !! x) of
                                       '#' -> Nothing
                                       ' ' -> nextCoordInDir f xs $ f (x, y, d)
                                       _ -> Just (x, y, d) 

step :: ((Int, Int, Dir) -> (Int, Int, Dir)) -> [String] -> [(Int, Int, Dir)] -> Move -> [(Int, Int, Dir)]
step _ xs ((x, y, d):ys) (F 0) = (x, y, d):ys
step f xs l (F z) = case nextCoordInDir f xs $ head l of
                      Just nl -> step f xs (nl:l) $ F (z - 1)
                      Nothing -> l
step _ xs ((x, y, d):ys) m = (x, y, turn m d):ys

score :: Dir -> Int
score DirR = 0
score DirD = 1
score DirL = 2
score DirU = 3

part1 :: ([Move],[String]) -> Int
part1 (moves, xs) = let start = findStart xs;
                        (x, y, d) = head $ foldl (step id xs) [start] moves
                    in 1000 * y + 4 * x + score d

cube :: [String] -> String
cube xs = unlines $ map (strip $ tail xs) [0..(length xs `div` 10)]
  where strip :: [String] -> Int -> String
        strip ys y = let l = ys !! (y * 10)
                     in map (\i -> l !! (i * 10)) [0..(length l `div` 10)]

mod50 :: Int -> Int
mod50 x = (x - 1) `mod` 50

wrap :: (Int, Int, Dir) -> (Int, Int, Dir)
wrap (x, 0, DirU) = if x <= 100 then (0, 151 + mod50 x, DirR) else (1 + mod50 x, 201, DirU)
wrap (x, 51, DirD) = (101, 51 + mod50 x, DirL)
wrap (x, 100, DirU) = (50, 51 + mod50 x, DirR)
wrap (x, 151, DirD) = (51, 151 + mod50 x, DirL)
wrap (x, 201, DirD) = (101 + mod50 x, 0, DirD)
wrap (0, y, DirL) = if y <= 150 then (50, 50 - mod50 y, DirR) else (51 + mod50 y, 0, DirD)
wrap (50, y, DirL) = if y <= 50 then (0, 150 - mod50 y, DirR) else (1 + mod50 y, 100, DirD)
wrap (51, y, DirR) = (51 + mod50 y, 151, DirU)
wrap (101, y, DirR) = if y <= 100 then (101 + mod50 y, 51, DirU) else (151, 50 - mod50 y, DirL)
wrap (151, y, DirR) = (101, 150 - mod50 y, DirL)
wrap x = error $ concat ["no pattern for: wrap ", show x]

dir :: Dir -> Char
dir DirU = '^'
dir DirR = '>'
dir DirD = 'v'
dir DirL = '<'

draw :: [String] -> (Int, Int, Dir) -> [String]
draw xs (x0, y0, d) = zipWith (\y ys -> zipWith (\x c -> if x0 == x && y0 == y then dir d else c) [0..] ys) [0..] xs

part2 :: ([Move],[String]) -> Int
part2 (moves, xs) = let start = findStart xs;
                        ys = foldl (step wrap xs) [start] moves;
                        (x, y, d) = head ys
                    --in unlines $ foldl draw xs ys
                    in 1000 * y + 4 * x + score d

