{-# OPTIONS_GHC -Wall #-}
module Igumnov06 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces s = if s == "" || not (isSpace (head s)) then s else spaces (tail s)
  
-- Задача 2.a ----------------------------------------- 
manyT :: String -> (String,String)
manyT "" = ("", "")
manyT s@(h:t) 
  | not (h == '<' || h == '>') = let (prefix, suffix) = manyT t
                                 in (h : prefix, suffix)
  | otherwise = ("", s)

-- Задача 2.b ----------------------------------------- 
value :: String -> (String,String)
value "" = ("", "")
value s@(h:t)
  | h /= '"' = let (prefix, suffix) = value t
               in (h : prefix, suffix)
  | otherwise = ("", s)

-- Задача 2.c ----------------------------------------- 
manyN :: String -> (String,String)
manyN "" = ("", "")
manyN s@(h:t)
  | not (isDigit h || isLetter h || h == '.' || h == '-') = ("", s)
  | otherwise = let (prefix, suffix) = manyN t
                in (h : prefix, suffix)

-- Задача 3.a -----------------------------------------
name :: String ->  Maybe(String,String) 
name "" = Nothing
name (h:t)
  | not (isLetter h) = Nothing
  | otherwise = let (prefix, suffix) = manyN t
                in Just (h : prefix, suffix)


-- Задача 3.b -----------------------------------------
text :: String ->  Maybe(String,String) 
text "" = Nothing
text (h:t) 
  | not (h == '<' || h == '>') = let (prefix, suffix) = manyT t
                                 in Just (h : prefix, suffix)
  | otherwise = Nothing

-- Задача 3.c -----------------------------------------
fullValue :: String ->  Maybe(String,String) 
fullValue "" = Nothing
fullValue (h:t)
  | h /= '"' = Nothing
  | otherwise = let (prefix, suffix) = value t
                in if null suffix then Nothing else Just (prefix, tail suffix) 

-- Задача 4.a -----------------------------------------
attrib :: String -> Maybe ((String,String),String) 
attrib s = case name s of 
             Just (npr, nsf) -> case spaces nsf of 
                                  ('=' : spsf) -> case fullValue (spaces spsf) of
                                                    Just (fvpr, fvsf) -> Just ((npr, fvpr), spaces fvsf)
                                                    Nothing -> Nothing
                                  _ -> Nothing
             Nothing -> Nothing

-- Задача 4.b -----------------------------------------
manyAtt :: String -> Maybe (Attributes,String) 
manyAtt s = case attrib s of 
              Just ((apr, asf), str) -> case manyAtt str of
                                         Just (avs, masf) -> Just ((apr, asf) : avs, masf)
                                         _ -> Nothing
              Nothing -> Just ([], s)

-- Задача 5.a -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag s = case s of
             ('<' : sf) -> case name sf of
                            Nothing -> Nothing
                            Just (npr, nsf) -> case manyAtt (spaces nsf) of
                                                Just (avs, '>' : masstr) -> Just ((npr, avs), masstr)
                                                _ -> Nothing
             _ -> Nothing

-- Задача 5.b -----------------------------------------
endTag :: String -> Maybe (String,String) 
endTag s = case s of 
             ('<' : ('/' : sf)) -> case name sf of
                                     Just (npr, '>' : nsf) -> Just (npr, nsf)
                                     _ -> Nothing
             _ -> Nothing
                            

-- Задача 6.a -----------------------------------------
element :: String -> Maybe (XML,String) 
element s = case begTag s of
              Just ((btnm, btavs), btsf) -> case manyXML btsf of
                                              Just (mxml, msf) -> case endTag msf of
                                                                    Just (etpr, etsf) -> if etpr == btnm
                                                                                           then Just (Element btnm btavs mxml, etsf)
                                                                                           else Nothing
                                                                    Nothing -> Nothing
                                              Nothing -> Nothing
              Nothing -> Nothing 

-- Задача 6.b -----------------------------------------
xml :: String -> Maybe (XML,String)
xml s = case element s of
          Nothing -> case text s of 
            Nothing -> Nothing
            Just (tpr, tsf) -> Just (Text tpr, tsf)
          Just (epr, esf) -> Just (epr, esf) 

-- Задача 6.c -----------------------------------------
manyXML :: String -> Maybe ([XML],String)
manyXML s = case xml s of
              Nothing -> case s of 
                           ('<' : ('/' : _)) -> Just ([], s)
                           _ -> Nothing
              Just (xxml, xstr) -> case manyXML xstr of
                                     Nothing -> Nothing
                                     Just (mxml, mstr) -> Just (xxml : mxml, mstr)

-- Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML s = case element (spaces s) of
              Nothing -> Nothing  
              Just (exml, str) -> if length (spaces str) > 0 
                                    then Nothing 
                                    else Just exml

-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]



