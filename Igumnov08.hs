{-# OPTIONS_GHC -Wall #-}
module Igumnov08 where

type AttName = String
type AttValue = String
type Attribute = (AttName, [AttValue])

type Header = [Attribute]
type Row = [AttValue]
type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

-- Допоміжні функції --------------------------------
indexOfAttribute :: AttName -> Header -> Int
indexOfAttribute at h = let occurances = [i | i <- [0 .. length h - 1], (fst $ h !! i) == at]
                        in if null occurances then -1 else head occurances

-- Задача 1 -----------------------------------------
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame [_] = True
allSame (l : ls) = l == head ls && allSame ls

-- Задача 2 -----------------------------------------
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove v abl = [(a, b) | (a, b) <- abl, a /= v]

-- Задача 3 -----------------------------------------
lookUpAtt :: AttName -> Header -> Row -> AttValue
--Передумова: Імя атрибуту присутнє в заданому заголовку.
lookUpAtt an hd row = row !! indexOfAttribute an hd

-- Задача 4 -----------------------------------------
removeAtt :: AttName -> Header -> Row -> Row
removeAtt an hd row = [r | r <- row, r /= lookUpAtt an hd row]

-- Задача 5 -----------------------------------------
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Передумова: Кожний рядок таблиці містить одне значення заданого атрибуту
buildFrequencyTable at ds = [(anv, length [True | rs <- [lookUpAtt (fst at) (fst ds) dt | dt <- snd ds], anv == rs]) | anv <- snd at] 

-- Задача 6 -----------------------------------------
nodes :: DecisionTree -> Int
nodes Null = 0
nodes (Leaf _) = 1
nodes (Node _ dt) = sum [nodes $ snd x | x <- dt] + 1

-- Задача 7 -----------------------------------------
evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _ = ""
evalTree (Leaf an) _ _ = an
evalTree (Node an als) hd row = let matches = [alsi | alsi <- als, fst alsi == lookUpAtt an hd row]
                                    nextTree = if null matches then Null else snd $ head matches
                                in evalTree nextTree hd row

-- Задача 8 -----------------------------------------
partitionData :: DataSet -> Attribute -> Partition
partitionData ds at = [(an, (remove (fst at) (fst ds), [removeAtt (fst at) (fst ds) x | x <- snd ds, lookUpAtt (fst at) (fst ds) x == an])) | an <- snd at]

-- Задача 9 -----------------------------------------
--
-- Задається...
-- В цьому простому випадку: атрибут, що вибирається - це перший атрибут в заголовку. 
--   Зауважимо, що кваліфікуючий атрибут присутній в заголовку,
--   тому його необхідно вилучити з можливих кандидатів. 
--
nextAtt :: AttSelector
--Передумова: Заголовок містить по крайній мірі один вхідний атрибут
nextAtt (headerDS, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) headerDS)

buildTree :: Attribute -> AttSelector -> DataSet -> DecisionTree 
buildTree _ _ (_, []) = Null
buildTree qat atSel ds = let qatvs = [lookUpAtt (fst qat) (fst ds) x | x <- snd ds]
                         in if allSame qatvs 
                              then Leaf $ head qatvs 
                              else Node (fst $ atSel ds qat) [(attv, buildTree qat atSel dtst) | (attv, dtst) <- partitionData ds (atSel ds qat)]

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- Це таж сама таблиця, але результат у другій колонці
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]

