module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "sample.txt"
        print . sum . take 3 . sortBy (flip compare) . map sum . map (map readInt) . splitWhen (\x -> x == "") . lines $ contents

readInt :: String -> Int
readInt = read

