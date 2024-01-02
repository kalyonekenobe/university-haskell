{-# OPTIONS_GHC -Wall #-}
module Igumnov07 where 

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne] 
                 deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne 
coef c p0 = if c /= 0 then map (\(a, b) -> (a, b * c)) p0 else []

-- Задача 1.b -----------------------------------------
add :: PolinomOne -> PolinomOne -> PolinomOne
add p0 p1 = unify $ p0 ++ p1

-- Задача 1.c -----------------------------------------
unify :: PolinomOne -> PolinomOne 
unify p0 = let orderVariables :: PolinomOne -> PolinomOne
               orderVariables [] = []
               orderVariables (p : po) = filter (\(ta, _) -> fst p > ta) (orderVariables po) ++
                                         (p : filter (\(ta, _) -> fst p <= ta) (orderVariables po))
           in orderVariables $ filter (\(_, b) -> b /= 0) $ 
                                 map (\(x, xs) -> (x, foldl (\t (_, b) -> t + b) 0 xs)) 
                                 [(y, filter (\(a, _) -> a == y) p0) | y <- unique [a | (a, _) <- p0]]     

-- Задача 2.a -----------------------------------------
findFree :: [PolinomOne] -> [Int]
findFree pos = [i + 1 | i <- [0 .. length pos - 1], length (pos !! i) == 1 && (fst $ head (pos !! i)) /= 0]

-- Задача 2.b -----------------------------------------
iswfCommon ::  [PolinomOne]  -> Bool 
iswfCommon pos = foldl (&&) True [foldl (\t (a, _) -> t && (a == 0 || (a `elem` (findFree pos)))) True pol | pol <- pos] 

-- Задача 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple le = foldl (&&) (not $ null le) [length l == 1 | l <- le] 

-- Задача 3.b -----------------------------------------
solveSimple :: Linear -> Maybe [PolinomOne] 
solveSimple le
  | foldl (&&) True [length l == 1 && head l == 0 | l <- le] = Just []
  | otherwise = Nothing

-- Задача 4.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow le = let res = [ind | ind <- [0 .. length le - 1], head (le !! ind) /= 0] in if null res then Nothing else Just $ head res + 1

-- Задача 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow le i
  | i == 0 || i > length le = le
  | otherwise = map (\(ind, x) -> if ind == i then le !! 0 else if ind == 1 then le !! (i - 1) else x) 
                    [(ind + 1, (le !! ind)) | ind <- [0 .. length le - 1]]

-- Задача 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep fs rs
  | any (\t -> length t /= length fs) rs = rs
  | otherwise = [[(r !! i) - (fs !! i) * (head r / head fs) | i <- [1 .. length r - 1]] | r <- rs] 

-- Задача 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep fs vs = (foldl add [(0, last fs / head fs)] [coef (-(fs !! (i + 1)) / head fs) (vs !! i) | i <- [0 .. length vs - 1]]) : vs

-- Задача 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne] 
gauss i le = if length le == 0 then Just []
                               else if isSimple le then solveSimple le
                                                   else case findRow le of
                                                          Nothing -> let nextLinear = [[(l !! j) | j <- [1 .. length l - 1]] | l <- le]
                                                                     in case gauss (i + 1) nextLinear of
                                                                          Nothing -> Nothing
                                                                          Just pol -> Just $ [(i, 1)] : pol 
                                                          Just j -> let exchanged = exchangeRow le j
                                                                        forwared = forwardStep (head exchanged) (tail exchanged)
                                                                    in case gauss (i + 1) forwared of
                                                                         Nothing -> Nothing
                                                                         Just nl -> Just $ reverseStep (head exchanged) nl
                                                    
-- Задача 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool 
testEquation pos rs = let result = addAll [coef (rs !! ind) (pos !! ind) | ind <- [0 .. length pos - 1]]
                      in length result == 1 && fst (result !! 0) == 0 && snd (result !! 0) == last rs

-- Задача 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool 
testLinear pos le = foldl (&&) True [testEquation pos l | l <- le]

-- Задача 8 -----------------------------------------
solving :: Linear -> Solution  
solving le = case gauss 1 ([0 | _ <- le !! 0] : le) of
               Just pos -> let polinomsCheck = [length pol == 1 && fst (pol !! 0) == 0 | pol <- pos]
                               singleAnswer = [snd (pol !! 0) | pol <- pos]
                               hasSingleAnswer = foldl (&&) True polinomsCheck 
                           in if not hasSingleAnswer then Many pos 
                                                     else One singleAnswer
               Nothing -> Empty

-- Додаткові функції --------------------------------
unique :: [Int] -> [Int]
unique [] = []
unique (x:xs)
  | x `elem` xs = unique xs
  | otherwise = x : unique xs

addAll :: [PolinomOne] -> PolinomOne
addAll pos = foldl add [] pos 

-------------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne 
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty 
sol2 = Empty 
sol3 = Many res3 
sol4 = One [62/15, -17/15, -4/3] 