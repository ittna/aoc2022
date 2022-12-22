module Main where

import Data.List
import Data.List.Split

main :: IO()
main = do  
        contents <- readFile "input.txt"
        print . part2 . parseInput . lines $ contents 

data Value = Constant String Int
           | Expression String [String] String (Int -> Int -> Int)

data Equation = Const Int
              | Var String 
              | Exp Equation String Equation

instance Show Equation where
  show (Const x) = show x
  show (Var x) = x
  show (Exp x op y) = concat ["(", show x, op, show y, ")"]

ident :: Value -> String
ident (Constant x _) = x
ident (Expression x _ _ _) = x

parseInput :: [String] -> [Value]
parseInput xs = map (parseValue . split (dropBlanks $ dropDelims $ oneOf ": ")) xs

parseValue :: [String] -> Value
parseValue (x:y:[]) = Constant x $ read y
parseValue (a:b:c:d:[]) = Expression a [b, d] c $ parseOp c

parseOp :: String -> (Int -> Int -> Int)
parseOp "+" = (+)
parseOp "-" = (-)
parseOp "*" = (*)
parseOp "/" = div

evaluate :: [Value] -> String -> Int
evaluate xs x = let y = findValue x xs
                in eval y
  where eval :: Value -> Int
        eval (Constant _ c) = c
        eval (Expression _ (a0:a1:as) _ f) = f (evaluate xs a0) (evaluate xs a1) 

findValue :: String -> [Value] -> Value
findValue x (y:ys) = if x == ident y
                     then y
                     else findValue x ys 

part1 :: [Value] -> Int
part1 xs = evaluate xs "root"

simplify :: [Value] -> String -> Equation
simplify xs x = let y = findValue x xs
                in simple y
  where simple :: Value -> Equation
        simple (Constant _ y) = Const y
        simple (Expression "humn" _ _ _) = Var "humn"
        simple (Expression x (a0:a1:as) op f) = case (simplify xs a0,simplify xs a1) of
                                                  (Const b0,Const b1) -> Const $ f b0 b1
                                                  (b0, b1) -> Exp b0 op b1

invertOp :: String -> Int -> Int -> Int
invertOp "+" x y = x - y
invertOp "-" x y = x + y
invertOp "*" x y = x `div` y
invertOp "/" x y = x * y

solve :: Equation -> Int
solve (Const x) = x
solve (Exp (Var _) op (Const x)) = x
solve (Exp (Exp x op (Const y)) "=" (Const z)) = solve $ Exp x "=" (Const $ invertOp op z y)
solve (Exp (Exp (Const y) "+" x) "=" (Const z)) = solve $ Exp x "=" (Const $ invertOp "+" z y) 
solve (Exp (Exp (Const y) "-" x) "=" (Const z)) = solve $ Exp x "=" (Const $ invertOp "+" y z) 
solve (Exp (Exp (Const y) "*" x) "=" (Const z)) = solve $ Exp x "=" (Const $ invertOp "*" z y) 
solve (Exp (Exp (Const y) "/" x) "=" (Const z)) = solve $ Exp x "=" (Const $ invertOp "/" y z) 

part2 :: [Value] -> Int
part2 xs = let (Expression _ as _ _) = findValue "root" xs;  
               ys = (Expression "humn" [] "" (+)):(Expression "root" as "=" (+)):(filter (\x -> not $ ident x `elem` ["root", "humn"]) xs)
           in solve $ simplify ys "root"

