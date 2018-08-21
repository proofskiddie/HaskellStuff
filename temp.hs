list1 :: Int -> Char -> String
list1 0 _ = []
list1 n a = a : list1 (n - 1) a

list2 :: Int -> String -> String -> [String]
list2 0 str _ = [str]
list2 n str a = list2 (n - 1) (a !! 0 : str) a ++ list2 (n - 1) (a !! 1 : str) a

listn' :: Int -> String -> String -> [String]
listn' 0 str a = [str]
listn' n str a = Prelude.foldr (++) [] (Prelude.map (\x -> listn' (n - 1) (x : str) a) a)

listn :: Int -> String -> [String]
listn n a = listn' n "" a

isReduce :: Char -> Char -> [(Char,Char)] -> Bool
isReduce 'e' '_' _ = True
isReduce  _  '_' _ = False
isReduce  a   b [] = False
isReduce a b ((x,y) : ss) | a == x && b == y = True
                          | otherwise        = isReduce a b ss

contains :: Eq a =>  a -> [a] -> Bool
contains _ [] = False
contains e (x:xs) = (e == x) || contains e xs

removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup (x:xs) = if contains x xs then removeDup xs else x : removeDup xs

grpReduce' :: Int -> String -> String -> Char -> [(Char, Char)] -> [String]
grpReduce' 0 str a _ _ = [str]
grpReduce' n str a p g = Prelude.foldr (++) [] (Prelude.map list a)
    where
        list    = (\x -> grpReduce' (n - 1) (fun x p g) a x g)
        fun x' p' g' = if (isReduce x' p' g') then str else (x' : str)

grpReduce :: Int -> String -> [(Char, Char)] -> [String]
grpReduce n alpha grp = removeDup $ grpReduce' n "" alpha '_' grp 

g1 = [('e', 'e')]
g2 = [('a', 'e'), ('e','a'), ('a','a')] ++ g1

grpMake :: String -> [(Char,Char)]
grpMake str  = (foldr (++) [] $  map (\x -> [(x, 'e'), ('e', x)]) str) ++ [('e','e')]

alpha n = take (n - 1) ['a'..]
g n = grpMake $ alpha n

easyGrp n = grpReduce n ('e' : alpha n) (g n)

