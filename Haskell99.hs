-- problem 1 last element of a list
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- problem 2 
myButLast :: [a] -> a
myButLast (x1:x2:[]) = x1
myButLast (x:xs) = myButLast xs

-- problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- problem 4
myLength :: [a] -> Int
myLength x = (fst . last) $ zip [1..] x

-- problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- problem 6
-- Eq allows using '==' on lists
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = foldr 
    (\(x, y) b -> if b then (if x == y then True else False) else False)
    True (zip xs (myReverse xs))

-- problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: Show a => NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = xs >>= flatten

-- problem 8
compress :: Eq a => [a] -> [a]
compress (x1:x2:xs) = if (x1 == x2) then compress (x2:xs) else x1 : compress (x2:xs)
compress x = x

-- problem 9
pack :: Eq a => [a] -> [[a]]
pack (x1:x2:xs) = if (x1 == x2) then (x1:(head s)):(tail s) else [x1]:s
    where s = pack (x2:xs)
pack x = [x]

-- problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\l -> (length l, head l)) (pack xs)

main = do
  print "-----"
  print "myLast"
  print "-----"
  print $ show (myLast [1,2,3,4]) 
  print $ show (myLast ['x','y','z']) 
  print "4"
  print "z"
  print "-----"
  print "myButLast"
  print "-----"
  print $ show (myButLast [1,2,3,4]) 
  print $ show (myButLast ['a'..'z']) 
  print "3"
  print "y"
  print "-----"
  print "elementAt"
  print "-----"
  print $ show (elementAt [1,2,3] 2)
  print $ show (elementAt "haskell" 5)
  print "2"
  print "e"
  print "-----"
  print "myLength"
  print "-----"
  print $ show (myLength [123,456,789])
  print $ show (myLength "Hello, world!")
  print "3"
  print "13"
  print "-----"
  print "myReverse"
  print "-----"
  print $ show (myReverse "A man, a plan, a canal, panama!")
  print $ show (myReverse [1,2,3,4])
  print "!amanap ,lanac a ,nalp a ,nam A"
  print "[4,3,2,1]"
  print "-----"
  print "isPalindrome"
  print "-----"
  print $ show (isPalindrome "madamimadam")
  print $ show (isPalindrome [1,2,4,8,16,8,4,2,1])
  print "True"
  print "True"
  print "-----"
  print "flatten"
  print "-----"
  print $ show (flatten (Elem 5))
  print $ show (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
  print $ show (flatten (List ([] :: [NestedList Int])))
  print "[5]"
  print "[1,2,3,4,5]"
  print "[]"
  print "-----"
  print "compress"
  print "-----"
  print $ show (compress "aaaabccaadeeee")
  print "abcade"
  print "-----"
  print "pack"
  print "-----"
  print $ show (pack "aaaabccaadeeee")
  print "[\"aaaa\",\"b\",\"cc\",\"d\",\"eeee\"]"
  print "-----"
  print "encode"
  print "-----"
  print $ show (encode "aaaabccaadeeee")
  print "[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]"


