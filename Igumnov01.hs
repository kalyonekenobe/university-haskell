{-# OPTIONS_GHC -Wall #-}
module Igumnov01 where

-- Задача 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n = if n >= 0 
  then if n == 0 
    then 1 
    else n * factorial (n - 1) 
  else undefined

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if getLength xs < getLength ys 
  then listSum (concatLists [xs, [0]]) ys 
  else if getLength xs > getLength ys
    then listSum xs (concatLists [ys, [0]])
    else if not (null xs) && not (null ys) 
      then (head xs + head ys) : listSum (tail xs) (tail ys) 
      else []

-- Задача 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven xs = swapElementsWithEvenOddIndexes xs []

-- Задача 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position n xs = findIndex n (getLength xs) xs
                     
-- Задача 5 -----------------------------------------
set :: [Int] -> [Int] 
set xs = buildSetFromList xs []

-- Задача 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (concatLists [xs, ys])

-- Задача 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = set [x | x <- xs, position x ys > -1]

-- Задача 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1 ..]]

-- Допоміжні функції --------------------------------
getLength :: [Int] -> Int
getLength xs = if null xs then 0 else 1 + getLength (tail xs)

concatLists :: [[Int]] -> [Int]
concatLists xss = [x | xs <- xss, x <- xs]

findIndex :: Int -> Int -> [Int] -> Int
findIndex n lngxs xs = if null xs 
  then -1
  else if n == head xs
    then lngxs - getLength (tail xs) - 1
    else findIndex n lngxs (tail xs)
  
swapElementsWithEvenOddIndexes :: [Int] -> [Int] -> [Int]
swapElementsWithEvenOddIndexes xs res = if null xs
  then res
  else if null (tail xs)
    then swapElementsWithEvenOddIndexes (tail xs) (concatLists [res, [head xs]])
    else swapElementsWithEvenOddIndexes (tail (tail xs)) (concatLists [res, [head (tail xs), head xs]])


buildSetFromList :: [Int] -> [Int] -> [Int]
buildSetFromList xs res = if getLength xs == 0
  then res
  else if position (head xs) res == -1
    then buildSetFromList (tail xs) (concatLists [res, [head xs]])
    else buildSetFromList (tail xs) res