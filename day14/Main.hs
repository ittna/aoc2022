module Main where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [[(Int, Int)]]
parseInput xs = map (map parsePair . splitOn " -> ") xs

parsePair :: String -> (Int, Int)
parsePair xs = let ys = splitOn "," xs in (read (ys !! 0), read (ys !! 1))

findRect :: [(Int, Int)] -> (Int, Int, Int, Int)
findRect cs = let (xs, ys) = (map fst cs, map snd cs);
                  (minX, minY) = (minimum xs, minimum ys);
                  (maxX, maxY) = (maximum xs, maximum ys)
              in (minX, minY, maxX - minX, maxY - minY) 

replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs

drawAt :: (Int, Int) -> Char -> [String] -> [String]
drawAt (x, y) c xs = let row = xs !! y; newRow = replaceAtIndex x c row
                   in replaceAtIndex y newRow xs

drawLine :: Char -> [String] -> (Int, Int, Int) -> [String]
drawLine c xs (y, x0, x1) = let l = xs !! y;
                                nl = take x0 l ++ (take (x1 - x0 + 1) $ repeat c) ++ drop (x1 + 1) l
                            in take y xs ++ [nl] ++ drop (y+1) xs

pairwise :: [a] -> [(a, a)]
pairwise (x:[]) = []
pairwise (x0:x1:xs) = (x0, x1):pairwise (x1:xs)

blockedAt :: [String] -> (Int, Int) -> Bool
blockedAt xs (x, y) = let c = (xs !! y) !! x in c == '#' || c == 'o'

dropSand :: [String] -> (Int, Int) -> Maybe (Int, Int)
dropSand xs (x, y) = if blockedAt xs (x, y) || x == length (head xs) - 1
                     then Nothing
                     else if blockedAt xs (x + 1, y)
                          then if blockedAt xs (x + 1, y - 1)
                               then if blockedAt xs (x + 1, y + 1)
                                    then Just (x, y)
                                    else dropSand xs (x + 1, y + 1)
                               else dropSand xs (x + 1, y - 1)
                          else dropSand xs (x + 1, y)

animateSand :: [String] -> (Int, Int) -> [String]
animateSand xs xy = let sand = dropSand xs xy
                    in if isNothing sand
                       then xs
                       else animateSand (drawAt (fromJust sand) 'o' xs) xy

part1 :: [[(Int, Int)]] -> Int
part1 xs = let (x, y, width, height) = findRect $ concat xs;
               (originX, originY) = (x - 1, 0);
               board = drawAt (500 - originX, 0) '+' $ take (y + height + 3) $ repeat $ take (width + 3) $ repeat '.';
               ls = concat $ map pairwise xs;
               hls = mapMaybe (\((x0, y0), (x1, y1)) -> if y0 == y1 then Just (y0, minimum [x0, x1] - originX, maximum [x0, x1] - originX) else Nothing) ls;
               vls = mapMaybe (\((x0, y0), (x1, y1)) -> if x0 == x1 then Just (x0 - originX, minimum [y0, y1], maximum [y0, y1]) else Nothing) ls;
               cave = foldl (drawLine '#') (transpose $ foldl (drawLine '#') board hls) vls
           in length $ filter (\c -> c == 'o') $ concat $ animateSand cave (0, 500 - originX)

part2 :: [[(Int, Int)]] -> Int
part2 xs = let (x, y, width, height) = findRect $ concat xs;
               h = y + height + 3;
               (originX, originY) = (500 - h, 0);
               board = drawLine '#' (drawAt (500 - originX, 0) '+' $ take h $ repeat $ take (2 * h + 1) $ repeat '.') (h - 1, 0, 2 * h);
               ls = concat $ map pairwise xs;
               hls = mapMaybe (\((x0, y0), (x1, y1)) -> if y0 == y1 then Just (y0, minimum [x0, x1] - originX, maximum [x0, x1] - originX) else Nothing) ls;
               vls = mapMaybe (\((x0, y0), (x1, y1)) -> if x0 == x1 then Just (x0 - originX, minimum [y0, y1], maximum [y0, y1]) else Nothing) ls;
               cave = foldl (drawLine '#') (transpose $ foldl (drawLine '#') board hls) vls
           in length $ filter (\c -> c == 'o') $ concat $ animateSand cave (0, 500 - originX)

