{-# OPTIONS_GHC -Wall #-}
module Igumnov10 where

--  Пакет parsec може бути закритим (hidden), 
--  щоб відкрити його потрібно завантажити файл з опцією -package parsec
--  ghci xxxx.hs -package parsec // ghc xxxx.hs -package parsec 

import Text.Parsec.String 
import Text.Parsec   --- parse

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- Задача 1 ------------------------------------ 
isNumbConst :: System -> Recur -> Bool 
isNumbConst syst f = case f of 
                       Name n -> let sr = [snd s | s <- syst, fst s == n]
                                 in if not $ null sr then isNumbConst syst $ sr !! 0 else False
                       Super Succ [ff] -> evRank syst f == fromEnum True && isNumbConst syst ff 
                       Super Zero [ff] -> evRank syst f == fromEnum True && isNumbConst syst ff
                       Sel _ _ -> True
                       Zero -> True
                       _ -> False  


-- Задача 2 ------------------------------------
evRank :: System -> Recur -> Int 
evRank syst f = case f of 
                  Zero -> 1
                  Succ -> 1
                  Sel a _ -> a
                  Super _ b -> evRank syst $ b !! 0
                  Prim _ b -> -1 + evRank syst b
                  Mini a _ -> -1 + evRank syst a
                  Name n -> let sr = [snd s | s <- syst, fst s == n]
                            in if not $ null sr then evRank syst $ sr !! 0 else 0

-- Задача 3 ------------------------------------
isNames :: System -> Bool 
isNames syst = let f :: System -> [String] -> Bool
                   f [] _ = True
                   f (s:ss) nms = fst s `notElem` nms && f ss (fst s : nms) && and [n `elem` nms | n <- fnnms $ snd s]
               in f syst []

-- Задача 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur syst f = let cs :: Recur -> Bool
                     cs ff = case ff of 
                               Zero -> True
                               Succ -> True
                               Name _ -> True
                               Sel a b -> a >= b
                               Super a b -> cs a && and [cs r | r <- b] && issm [evRank syst c | c <- b]
                               Prim a b -> let rk = evRank syst b - evRank syst a 
                                           in cs a && cs b && if not $ isNumbConst syst a then rk == 2 else rk == 1 || rk == 2
                               Mini a b -> evRank syst ff > 1 && b >= 0 && cs a
                 in isNames syst && cs f

-- Задача 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int 
eval syst f vl = case f of 
                   Zero -> 0
                   Succ -> 1 + vl !! 0
                   Sel _ b -> vl !! (b - 1) 
                   Name n -> let sr = [snd s | s <- syst, fst s == n]
                             in if not $ null sr then eval syst (sr !! 0) vl else 0
                   Super a b -> eval syst a [eval syst bb vl | bb <- b]
                   Prim a b -> let ael = take (length vl - 1) vl
                                   lst = vl !! (length vl - 1)
                               in if lst == fromEnum False 
                                   then eval syst a ael
                                   else eval syst b (concat [ael, [lst - 1], [eval syst f (concat [ael, [lst - 1]])]])
                   Mini _ _ -> case evalPart syst f vl of
                                 Nothing -> 0
                                 Just a -> a

-- Задача 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart syst f vl = case f of 
                       Name n -> let sr = [snd s | s <- syst, fst s == n]
                                 in if not $ null sr then evalPart syst (sr !! 0) vl else Nothing
                       Mini a b -> em (concat [vl, [0]]) syst a b
                       _ -> Just (eval syst f vl)

-- Задача 7 ------------------------------------
parseRec :: String -> Maybe System 
parseRec str = case parse sys "" [s | s <- str, s `notElem` " \t\r\n\v\f"] of
                 Right a -> Just a
                 _ -> Nothing

-- Допоміжні функції ---------------------------
fnnms :: Recur -> [String]
fnnms f = case f of 
            Mini a _ -> fnnms a
            Name n -> [n]
            Super a b -> concat [fnnms c | c <- a : b]
            Prim a b -> concat [fnnms c | c <- [a, b]]
            _ -> []

issm :: [Int] -> Bool
issm [] = True
issm (x : xs) = and [i == x | i <- xs]

em :: [Int] -> System -> Recur -> Int -> Maybe Int
em vl syst f ind = let lst = vl !! (length vl - 1)
                       ael = take (length vl - 1) vl
                   in if lst <= ind
                        then case evalPart syst f vl of
                               Just 0 -> Just lst
                               Nothing -> Nothing
                               _ -> em (concat [ael, [lst + 1]]) syst f ind
                        else Nothing

sys :: Parser System
sys = do a <- many innx; eof; return a

cc :: Parser Recur
cc = do _ <- char ','; a <- rc; return a

idd :: Parser Recur
idd = do a <- letter; b <- many (digit <|> letter); return (Name $ a : b)

fz :: Parser Recur
fz = do _ <- string "z1"; return Zero

innx :: Parser (String, Recur)
innx = do i <- inn; _ <- char '='; a <- rc; _ <- char ';'; return (i, a)

int :: Parser Int
int = do a <- many1 digit; return (read a)

fsdd :: Parser Recur
fsdd = do _ <- char 's'; a <- digit; b <- digit; return (Sel (read [a]) (read [b]))

bs :: Parser Recur
bs = try fz <|> try fs <|> try fsdd <|> idd

mn :: Parser Recur
mn = do _<- char '{'; a <- rc; _ <- char ','; b <- int; _ <- char '}'; return (Mini a b) 

fs :: Parser Recur
fs = do _ <- string "a1"; return Succ

inn :: Parser String
inn = do y <- letter; z <- many (digit <|> letter); return (y : z)

sp :: Parser Recur
sp = do _ <- char '('; a <- rc; _ <- char ':'; b <- rc; c <- many cc; _ <- char ')'; return (Super a (b : c))

pr :: Parser Recur
pr = do _ <- char '['; a <- rc; _ <- char ','; b <- rc; _ <- char ']'; return (Prim a b)

rc :: Parser Recur
rc = bs <|> sp <|> pr <|> mn

---------------------Тестові дані -  -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
