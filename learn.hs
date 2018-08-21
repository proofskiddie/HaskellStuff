
-- foldr applies function id to every element of the list
-- 
myLast :: [a] -> a
myLast = foldr1 (const  id)

myButLast :: [a] -> a
myButLast l = l !! (length l - 2)

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt xs n | n <= 0    = error "positive n"
               | n == 1    = head xs
               | otherwise = elementAt (tail xs) (n - 1)

mylen :: [a] -> Int
mylen [] = 0
mylen (x:xs) = 1 + mylen xs


myLength :: [a] -> Int
myLength xs = snd (last (zip xs [1..]))

myLen :: [a] -> Int
myLen xs = foldr (const (+1)) 0 xs

mylent :: [a] -> Int
mylent = fst . last . zip [1..]

rev :: [a] -> [a]
rev xs = foldr (\x f e -> f (x : e)) id xs []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome (x : xs) | x == last xs    = isPalindrome $ init xs
                      | otherwise   = False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = foldr (\x y -> (fst x == snd x) && y) True (zip xs (reverse xs))

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

flat :: NestedList b -> [b]
flat (Elem x) = return x
flat (List x) = flatten =<< x

toDigits :: Integer -> [Integer]
toDigits n | n <= 0     = []
           | otherwise  = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0    = []
              | otherwise = (mod n 10) : toDigits (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith fun ys xs
          where   fun ys xs = (\x y -> f x y) ys xs
                  f x y = (if even x then 2 * y else y)
                  ys = let m = length xs in [m, (m - 1) .. 1]

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ foldr (++) [] $ map toDigits xs

validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0

fac :: Integer -> Integer
fac n = foldr (*) 1 [1..n]

data Person = Person Int String
  deriving Show

baz :: Person -> String
baz p@(Person n _) = show p


llen :: [a] -> Int
llen xs = snd $ last $ zip xs [1..]

dub = zipWith (\x y -> if x `mod` 2 == 0 then 2 * y else y) [1..] 
