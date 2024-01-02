{-# OPTIONS_GHC -Wall #-}
module Igumnov00 where

data Command = Z Int | S Int | T Int Int | J Int Int Int  deriving (Show, Eq)
type Program = [Command]
type ConfigС = (Int, Int, [Int])

type Graph  = [[Int]]

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 

-- Задача  1 -----------------------------------------
maximReg :: Program -> Int 
maximReg x = if length x == 0 then length x
                              else let lval = case x !! 0 of
                                                Z a -> a
                                                S a -> a
                                                T a b -> if a > b then a else b
                                                J a b _ -> if a > b then a else b 
                                       rval = maximReg $ tail x
                                   in if lval > rval then lval else rval

-- Задача 2 -----------------------------------------
ini :: Program -> [Int] -> [Int] 
ini a b = [if i < length b then b !! i else 0 | i <- [0 .. maximReg a - 1]]

upd :: [Int] -> Int -> Int-> [Int]
upd a b c = if b < 0 || b >= length a then a else [if i /= b then a !! i else c | i <- [0 .. length a - 1]] 

-- Задача 3 -----------------------------------------
stepC :: Program -> ConfigС -> ConfigС
stepC a (x, y, z) = stc (a !! (x - 1)) (x, y, z) 

--- Задача 4 ----------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC a b c = let f :: [Int] -> (Int, Int) -> Maybe Int 
                  f u (v, w) = let (x, y, z) = stepC a (v, w, u) in if w == b then Nothing else if x > length a then Just $ head z else f z (x, y)
              in f (ini a c) (1, 0)

-- Задача 5 -----------------------------------------
palindrom10 :: Int -> Int -> [Int] 
palindrom10 n m = let sp :: Int -> [Int]
                      ip :: Int -> Bool
                      sp x = if x > 0 then sp (x `div` 10) ++ [(x `mod` 10)] else []
                      ip x = sp x == reverse (sp x)
                  in [x | x <- [n + 1 .. m - 1], ip x]

-- Задача 6 -----------------------------------------
maxSuf :: [Int] -> Int
maxSuf xs = let f :: [Int] -> (Int, Int)
                f [] = (0, 0)
                f [x] = (x, x)
                f (x : xxs) = let val = f xxs
                                  sm = fst val + x 
                                  mxsm = snd val
                              in if mxsm < sm then (sm, sm) else (sm, mxsm)
            in snd (f xs)

-- Задача 7 -----------------------------------------
encode :: String -> [(Int,Char)]
encode [] = []
encode (x : xs) = let exs = encode xs
                  in if length exs > 0 && snd (head exs) == x then (fst (head exs) + 1, x) : tail exs else (1, x) : exs

-- Задача 8 -----------------------------------------
maxComSuf :: String -> String -> Int
maxComSuf xs ys = let f :: Int -> Int
                      f i = let mxsz = if length xs > length ys then length xs - 1 else length ys - 1
                            in if i > mxsz then mxsz
                              else if i < 0 then 0
                              else if i >= length xs || i >= length ys || xs !! (length xs - i - 1) /= ys !! (length ys - i - 1) then i - 1
                              else f (i + 1)
                  in 1 + f 0

-- Задача 9 -----------------------------------------
groupChar :: String -> [String] 
groupChar xs = [[snd it | _ <- [1 .. fst it]] | it <- encode xs]

--- Задача 10 ----------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity n m = let f :: SW
                       f u v w 
                         | v /= w = let hat = aww u v; ws = [dsa | fag <- hat, dsa <- reverse fag, w == dsa !! 0] in if length ws > 0 then reverse $ ws !! (length ws - 1) else [] 
                         | otherwise = []
                   in fmx [length (f n m w) - 1 | w <- lar n]

--- Задача 11 ----------------------------------------
findDiameter :: Graph -> Int 
findDiameter n = fmx [eccentricity n qt | qt <- lar n]

findRadius :: Graph -> Int 
findRadius n = fmn [eccentricity n qt | qt <- lar n]

--- Задача 12 ----------------------------------------
findCenter :: Graph -> [Int] 
findCenter n = [qt | qt <- lar n, findRadius n == eccentricity n qt]

--- Задача 13 ----------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]] 
shortWayAll u v w
  | v /= w = let ghga = concat $ aww u w; hf = filter (\x -> head x == v) ghga 
             in if length hf > 0 then [h | h <- hf, length h == length (hf !! (length hf - 1))] else [] 
  | otherwise = []

--- Задача 14 ----------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v o l r) = let lb = case l of 
                                      EmptyM -> True
                                      NodeM lv lo _ _ -> lv < v && lo > 0
                               rb = case r of 
                                      EmptyM -> True
                                      NodeM rv ro _ _ -> rv > v && ro > 0
                               chk :: (Ord a) => BinTreeM a -> a -> Bool
                               chk EmptyM _ = False
                               chk (NodeM tv _ ll rr) x = tv == x || chk ll x || chk rr x
                           in o > 0 && lb && rb && isSearch l && isSearch r && (not (chk l v)) && (not (chk r v))

--- Задача 15 ----------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM x _ l r) xx = if x /= xx then elemSearch (if xx < x then l else r) xx else True

--- Задача 16 ----------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM xx = NodeM xx 1 EmptyM EmptyM 
insSearch (NodeM x o l r) xx = if x == xx then NodeM x (o + 1) l r
                               else if xx < x then if l == EmptyM then NodeM x o (NodeM xx 1 EmptyM EmptyM) r else NodeM x o (insSearch l xx) r
                               else if r == EmptyM then NodeM x o l (NodeM xx 1 EmptyM EmptyM) else NodeM x o l (insSearch r xx)

--- Задача 17 ----------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch node xx = delSearchExt node xx False

delSearchExt :: (Ord a) => BinTreeM a -> a -> Bool -> BinTreeM a
delSearchExt EmptyM _ _ = EmptyM
delSearchExt (NodeM x o l r) xx del = if x == xx then if o > 1 && not del 
                                                   then NodeM x (o - 1) l r 
                                                   else let fcls :: BinTreeM a -> BinTreeM a
                                                            fcls EmptyM = EmptyM
                                                            fcls (NodeM xxx oo ll rr) = case ll of 
                                                                                          EmptyM -> NodeM xxx oo ll rr
                                                                                          _ -> fcls ll
                                                            fnd = fcls r
                                                        in case fnd of 
                                                             EmptyM -> EmptyM
                                                             _ -> let (NodeM xxx oo _ _) = fnd
                                                                  in NodeM xxx oo l (delSearchExt r xxx True)
                                      else if xx < x && l /= EmptyM then NodeM x o (delSearchExt l xx del) r
                                      else if xx > x && r /= EmptyM then NodeM x o l (delSearchExt r xx del)
                                      else (NodeM x o l r)

--- Задача 18 ----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList x = let f :: (Ord a) => BinTreeM a -> [a]
                 f EmptyM = []
                 f (NodeM xx o l r) = f l ++ [xx | _ <- [1 .. o]] ++ f r
             in f (foldl insSearch EmptyM x)

--- Допоміжні функції --------------------------------
type Ways = [[[Int]]]
type AW = Graph -> Int -> Ways
type SW = Graph -> Int -> Int -> [Int]
type AP = Graph -> Int -> Int -> [[Int]]

stc :: Command -> ConfigС -> ConfigС
stc (T u v) (x, y, z) = (1 + x, 1 + y, upd (upd z (v - 1) (z !! (u - 1))) (u - 1) (z !! (v - 1)))
stc (Z u) (x, y, z) = (1 + x, 1 + y, upd z (u - 1) 0)
stc (J u v w) (x, y, z) = if z !! (u - 1) /= z !! (v - 1) 
                             then (1 + x, 1 + y, z) 
                             else (w, 1 + y, z)
stc (S u) (x, y, z) = (1 + x, 1 + y, upd z (u - 1) (z !! (u - 1) + 1))

aww :: AW
aww x y = let f :: Graph -> Ways -> Ways
              c :: Ways -> Bool
              w = [[[y]]]
              f a b = [cc : o | o <- head b, head o `notElem` tail o, cc <- a !! head o] : b
              c a = length (head a) == 0
              res = until c (f x) w
          in res

fmx :: [Int] -> Int
fmx [] = 0
fmx [x] = x
fmx (x : xs) = let res = fmx xs in if x > res then x else res

fmn :: [Int] -> Int
fmn [] = 0
fmn [x] = x
fmn (x : xs) = let res = fmn xs in if x < res then x else res

lar :: Graph -> [Int] 
lar x = [0 .. length x - 1]

---------------------Тестові дані 
---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]

---------------------- Графи -------
gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

---  Бінарні дерева 
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   
