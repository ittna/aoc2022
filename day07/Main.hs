module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents

parseInput :: [String] -> [([String], [String])]
parseInput xs = map parseCommand $ (split $ dropInitBlank $ keepDelimsL $ whenElt (\x -> head x == '$')) xs

parseCommand :: [String] -> ([String], [String])
parseCommand (x:xs) = (drop 1 $ splitOn " " x, xs)

parseSize :: String -> Int
parseSize output = if head output == 'd'
                   then 0 
                   else read $ head $ splitOn " " output 

cd :: [String] -> [String] -> [String]
cd pwd ("/":xs) = []
cd pwd ("..":xs) = drop 1 pwd
cd pwd (x:xs) = x:pwd

ls :: [String] -> [String] -> (String, Int)
ls pwd output = ("/" ++ (intercalate "/" $ reverse pwd), sum $ map parseSize output)

execute :: ([String], [(String, Int)]) -> ([String], [String]) -> ([String], [(String, Int)])
execute (pwd, xs) (cmd, output) = if head cmd == "cd"
                                  then (cd pwd $ tail cmd, xs)
                                  else (pwd, (ls pwd output):xs) 

part1 :: [([String], [String])] -> Int
part1 xs = let (pwd, dirs) = foldl execute ([], []) xs
           in sum $ map snd $ filter (\ (_, size) -> size <= 100000) $ foldl (\ xs (path, _) -> (path, sum $ map snd $ filter (\ (p, _) -> isPrefixOf path p) dirs):xs) [] dirs

part2 :: [([String], [String])] -> Int
part2 xs = let (pwd, dirs) = foldl execute ([], []) xs
               cdirs = foldl (\ xs (path, _) -> (path, sum $ map snd $ filter (\ (p, _) -> isPrefixOf path p) dirs):xs) [] dirs;
               used = maximum $ map snd cdirs;
               unused = 70000000 - used;
               needed = 30000000 - unused
           in minimum $ map snd $ filter (\ (_, size) -> size >= needed) cdirs

