{-# OPTIONS_GHC -Wall #-}
module Igumnov04 where

-- Mastermind -----------------------------------------

-- Фішка може мати один з шести кольорів
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- Код - просто список фішок
type Code = [Peg]

-- Крок гри (Move) будує конструктор Move використовуючи код (Code) і два цілих;  
-- кількість повних і часткових відповідностей кода-пропозиції і шифру
data Move = Move Code Int Int deriving (Show, Eq)

-- Список містить всі різні допустимі кольори
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (c:cd) (p:pr) = exactMatches cd pr + if c == p then 1 else 0

-- Задача 2 -----------------------------------------
countColors :: Code -> [Int]
countColors cd = [countOccurrences cd c | c <- colors]
countOccurrences :: Code -> Peg -> Int
countOccurrences [] _ = 0
countOccurrences (c:cd) p = countOccurrences cd p + if c == p then 1 else 0 

-- Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd pr = let countMatches :: Code -> Code -> Code -> Int
                    countMatches [] _ _ = 0
                    countMatches _ [] _ = 0
                    countMatches ccd (p:ppr) cppr = if head ccd == p 
                      then 1 + countMatches (tail ccd) (cppr ++ ppr) [] 
                      else 0 + if null ppr 
                        then countMatches (tail ccd) (cppr ++ [p] ++ ppr) []
                        else countMatches ccd ppr (cppr ++ [p])
                in countMatches cd pr []
 
-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd pr = let x = exactMatches cd pr
                in Move pr x (matches cd pr - x)

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move pr f p) cd = let x = exactMatches pr cd 
                                in f == x && p == matches pr cd - x

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cs = filter (isConsistent mv) cs

-- Задача 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concatMap (\x -> [y:x | y <- colors]) (allCodes (n - 1))
   
-- Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = let getSolution :: [Code] -> [Move] -> [Move]
               getSolution cds mvs = let m = getMove cd (head cds)
                                         (Move _ f p) = m
                                     in if p == 0 && f == length cd 
                                          then mvs ++ [m]
                                          else getSolution (filterCodes m cds) (mvs ++ [m]) 
           in getSolution (allCodes 4) []
 
