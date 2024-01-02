{-# OPTIONS_GHC -Wall #-}
module Igumnov02 where

-- Задача 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs
  
-- Задача 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

-- Задача 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys = foldr (:) ys xs

-- Задача 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert xs = foldl insert [] xs
insert :: [Integer] -> Integer -> [Integer]
insert xs v = concatFr (concatFr [x | x <- xs, x < v] [v]) [x | x <- xs, x >= v]

-- Задача 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) = (f x y):(map2 f xs ys)   

-- Задача 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum [fromIntegral m ^ i / fromIntegral (factorial i) | i <- [1..n]]

-- Задача 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Задача 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [x * x | x <- [1..]]

-- Задача 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys = let findIndexes::[Int] -> Int -> [Int]
                    findIndexes yys i = if i <= length ys 
                                          then if xs == take (length xs) yys
                                            then i:(findIndexes (tail yys) (i + 1))
                                            else findIndexes (tail yys) (i + 1)
                                        else []
                in findIndexes ys 0  

