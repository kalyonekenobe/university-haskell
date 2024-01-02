{-# OPTIONS_GHC -Wall #-}
module Igumnov03 where

-- Задача 1 -----------------------------------------
testing :: [Int] -> Bool
testing [] = True
testing [_] = True
testing (x:xs) = x <= head xs && testing xs

-- Задача 2 -----------------------------------------
all35 :: Int -> [Int]
all35 n = [x | x <- [1..(n - 1)], x `mod` 3 == 0, x `mod` 5 == 0]

-- Задача 3 -----------------------------------------
compress :: [Int] -> [Int]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs then compress xs else x:compress xs

-- Задача 4 -----------------------------------------
primeFactor :: Int -> [Int]
primeFactor n = if n < 2 then [] 
                  else let y = greatestPrimeDivider n
                       in primeFactor (n `div` y) ++ [y]
greatestPrimeDivider :: Int -> Int
greatestPrimeDivider n = let f :: [Int] -> [Int]
                             f [] = []
                             f (x:xs) = x : f (filter (\y -> y `mod` x /= 0) xs)
                         in last (f (2 : [x | x <- [3, 5..n], n `mod` x == 0]))
  
-- Задача 5 ----------------------------------------- 
fibons :: [Integer]
fibons = 1 : 1 : zipWith (+) fibons (tail fibons)

-- Задача 6 -----------------------------------------
lastTail :: String -> String
lastTail [] = []
lastTail [a] = [a]
lastTail xs = max (lastTail (tail xs)) xs

-- Задача 7 -----------------------------------------
intToString :: Int -> Int -> String
intToString _ 0 = []
intToString 0 _ = []
intToString 1 _ = []
intToString n m = intToString n (m `div` n) ++ ["0123456789abcdefghijklmnopqrstuvwxyz" !! (m `mod` n)]

-- Задача 8 -----------------------------------------
sumPalindrom2 :: Integer -> Integer
sumPalindrom2 n = sum [x | x <- [1..n], toBinaryString x == reverse (toBinaryString x)] 
toBinaryString :: Integer -> String
toBinaryString 0 = []
toBinaryString n = toBinaryString (n `div` 2) ++ [if n `mod` 2 == 0 then '0' else '1']
