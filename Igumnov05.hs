{-# OPTIONS_GHC -Wall #-}
module Igumnov05 where

type Graph  = [[Int]]

allUnique :: [Int] -> Bool
allUnique [] = True
allUnique (x:xs) = (x `notElem` xs) && allUnique xs

vertices :: Graph -> [Int]
vertices gr = [0..length gr - 1]

bitor :: Bool -> Bool -> Bool
bitor False x = x
bitor True x = not x

closure :: Graph -> [Bool] -> Int -> Int -> Bool
closure gr visited u v
  | v `elem` (gr !! u) = True
  | not (visited !! u) = any (\w -> not (visited !! w) && closure gr (setVisited w visited) w v) (gr !! u)
  | otherwise = False

setVisited :: Int -> [Bool] -> [Bool]
setVisited u visited = take u visited ++ [True] ++ drop (u + 1) visited

findGraphCycles :: Graph -> Int -> Int -> [Int] -> Graph
findGraphCycles gr current final visited 
  | current == final && length visited > 0 = [[final]]
  | current `elem` visited = []
  | otherwise = [current : cycleVertices | next <- gr !! current, cycleVertices <- (findGraphCycles gr next final (current : visited))]

position :: Int -> [Int] -> Maybe Int
position y ls = let indexOf :: [Int] -> Int -> Maybe Int
                    indexOf xs ind
                      | null xs || ind >= length ls = Nothing
                      | y == head xs = Just ind
                      | otherwise = indexOf (tail xs) (ind + 1)
                in indexOf ls 0

-- Задача 1 ------------------------------------
isGraph :: Graph -> Bool
isGraph [] = True
isGraph (g:gr) = isGraph gr && allUnique g

-- Задача 2 ------------------------------------
isTournament :: Graph -> Bool 
isTournament gr = isGraph gr && length [(u, v) | u <- vertices gr, v <- vertices gr, u /= v, bitor (v `elem` (gr !! u)) (u `notElem` (gr !! v))] == 0

-- Задача 3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive gr = length [(u, v) | u <- vertices gr, v <- gr !! u, w <- gr !! v, w `notElem` gr !! u] == 0

-- Задача 4 ------------------------------------
buildTransitive :: Graph -> Graph 
buildTransitive gr = let transitiveClosure u v visited = v `elem` (gr !! u) || any (\w -> not (visited !! w) && transitiveClosure w v (setVisited w visited)) (gr !! u)
                     in [[vv | vv <- [0..(length gr - 1)], transitiveClosure uu vv (setVisited uu (replicate (length gr) False))] | uu <- [0..(length gr - 1)] ]

-- Задача 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr a b = let findAllWaysFromVertex :: Int -> [Int] -> Graph
                     findAllWaysFromVertex u visited 
                       | u == b = [[b]]
                       | otherwise = [u : w | v <- (gr !! u), v `notElem` visited, u /= v, w <- (findAllWaysFromVertex v (u : visited))]
                     findLongestWay :: Graph -> [Int]
                     findLongestWay [] = []
                     findLongestWay (g:grc) 
                       | length g >= length (findLongestWay grc) = g
                       | otherwise = findLongestWay grc
                     res = findLongestWay (findAllWaysFromVertex a [])
                 in if null res then Nothing else Just res
                 
                     
-- Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr = let graphCycles = [x | v <- [0..(length gr - 1)], x <- findGraphCycles gr v v [], length gr + 1 == length x]
                 in if length graphCycles == 0 then Nothing else Just (head graphCycles)

-- Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic gr = length ([x | v <- [0..(length gr - 1)], x <- findGraphCycles gr v v []]) == 0

-- Задача 8 ------------------------------------
type TopolSortState = (Int, [Int])

topolSort :: Graph -> Maybe [Int]
topolSort gr = let checkGraphIterated :: TopolSortState -> Bool
                   checkGraphIterated (u_ind, _) = length gr <= u_ind
                   updateAnswer :: TopolSortState -> TopolSortState
                   updateAnswer (u_ind, ans) = let loopCheck :: TopolSortState -> Bool
                                                   loopCheck (v_ind, _) = length (gr !! u_ind) <= v_ind
                                                   updateLoopAnswer :: TopolSortState -> TopolSortState
                                                   updateLoopAnswer (v_ind, l_ans) = (v_ind + 1, snd (updateAnswer (gr !! u_ind !! v_ind, l_ans)))
                                                   loop_ans = snd (until loopCheck updateLoopAnswer (0, ans))
                                               in if u_ind `notElem` loop_ans then (u_ind + 1, u_ind : loop_ans) else (u_ind + 1, loop_ans)
               in if isAcyclic gr then Just (snd (until checkGraphIterated updateAnswer (0, []))) else Nothing
     

-- Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort gr ts = (length ts == length gr) && allUnique ts && isAcyclic gr &&
                    (length [(u, v) | uu <- [0..(length gr) - 1], vv <- (gr !! uu), u <- [uu `position` ts], v <- [vv `position` ts], u == Nothing || v == Nothing || u > v] == 0) 

---------------------Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]