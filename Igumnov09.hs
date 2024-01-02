{-# OPTIONS_GHC -Wall #-}
module Igumnov09 where

import Data.Char(isLower)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat bdd env = if fst bdd == 0 then False else if fst bdd == 1 then True
                     else let nodes = [n | n@(ind, _) <- snd bdd, ind == fst bdd]
                              (_, (nm, f, t)) = if null nodes then (-1, (' ', -1, -1)) else head nodes
                              nextId = let envs = [e | e <- env, fst e == nm]
                                       in if null envs then -1 else if (snd $ head envs) then t else f
                          in if nextId == -1 then False else checkSat (nextId, snd bdd) env

-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat bdd = [res | res <- sequence [[(x, y) | y <- [False, True]] | x <- unique [n | (_, (n, _, _)) <- snd bdd]], checkSat bdd res]

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify e = case e of 
               Or (Bvalue a) (Bvalue b) -> Bvalue $ a || b
               Not (Bvalue x) -> Bvalue $ not x
               And (Bvalue a) (Bvalue b) -> Bvalue $ a && b
               _ -> e
               
-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict e x v = let f :: BExp -> BExp
                     f ee = case ee of 
                              Bvar y -> if y == x then Bvalue v else ee
                              Or a b -> simplify $ Or (f a) (f b)
                              And a b -> simplify $ And (f a) (f b)
                              Not a -> simplify $ Not $ f a
                              _ -> ee
                 in f e

-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' e n xs = case e of 
                     Bvalue False -> (0, [])
                     Bvalue True -> (1, [])
                     _ -> let ln = n * 2
                              rn = n * 2 + 1
                              f = restrict e (head xs) False
                              t = restrict e (head xs) True
                              (lf, ls) = buildBDD' f ln (tail xs)
                              (rf, rs) = buildBDD' t rn (tail xs)
                              it = (n, (head xs, lf, rf))
                              xxs = ls ++ rs
                          in (n, it : xxs)

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD e xs = let fn :: [BDDNode] -> BExp -> String -> Int -> BDD
                      fn ss ee vs n = case ee of 
                                        Bvalue b -> if b then (1, ss) else (0, ss)
                                        _ -> let ln = n * 2
                                                 rn = n * 2 + 1
                                                 f = restrict ee (head vs) False
                                                 t = restrict ee (head vs) True
                                                 (lf, ls) = fn ss f (tail vs) ln
                                                 (rf, rs) = fn ls t (tail vs) rn
                                                 fnd = [fst r | r <- rs, snd r == (head vs, lf, rf)]
                                                 nxt = if null fnd then (n, (n, (head vs, lf, rf)) : rs) else (head fnd, rs)
                                             in if lf == rf then (lf, ls) else nxt
                    in fn [] e xs 2

-- Задача 7 -----------------------------------------
fullBexp :: String -> Maybe BExp 
fullBexp s = case bexp s of 
               Just (e, "") -> Just e
               _ -> Nothing

bexp :: String -> Maybe (BExp,String)
bexp s = case bcon s of 
           Just ss -> manyCon ss
           Nothing -> Nothing

bcon :: String -> Maybe (BExp,String)
bcon s = case bdis s of 
           Just ss -> manyDis ss
           Nothing -> Nothing

manyCon :: (BExp,String) -> Maybe (BExp,String)
manyCon s = case s of 
              (e, "") -> Just (e, "")
              (e, '|' : rs) -> case bcon rs of 
                                 Just (ee, bs) -> manyCon (Or e ee, bs)
                                 Nothing -> Nothing
              _ -> Just s

bdis :: String -> Maybe (BExp,String)
bdis s
  | null s = Nothing
  | otherwise = case s of
                  ('(' : rs) -> case bexp rs of 
                                  Just (e, ')' : bs) -> Just (e, bs)
                                  _ -> Nothing
                  ('!' : rs) -> case bdis rs of 
                                  Just (e, bs) -> Just (Not e, bs) 
                                  Nothing -> Nothing
                  ('F' : rs) -> Just (Bvalue False, rs)
                  ('T' : rs) -> Just (Bvalue True, rs)
                  (c : rs) -> if (c `elem` ['a'..'z']) && (isLower c) then Just (Bvar c, rs) else Nothing
                  _ -> Nothing


manyDis :: (BExp,String) -> Maybe (BExp,String)
manyDis s = case s of 
              (e, "") -> Just (e, "")
              (e, '&' : rs) -> case bdis rs of 
                                 Just (ee, bs) -> manyDis (And e ee, bs)
                                 Nothing -> Nothing
              _ -> Just s
              

-- Додаткові функції ---------------------------------
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) 
  | x `notElem` unique xs = x : unique xs 
  | otherwise = unique xs

------------------------------------------------------
-- Приклади для тестування..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])



