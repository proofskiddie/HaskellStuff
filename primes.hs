
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k + 1) n

ld :: Integer -> Integer        
ld n = ldf 2 n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n 


minl :: [Integer] -> Integer
minl [] = error "empty list"
minl [x] = x
minl (x:xs) | x < (minl xs) = x
            | otherwise     = (minl xs)

maxl :: [Integer] -> Integer
maxl [] = error "empty list"
maxl [x] = x
maxl (x:xs) | x > (maxl xs) = x
            | otherwise     = (maxl xs)

removeFst :: Integer ->  [Integer] -> [Integer]
removeFst n [] = []
removeFst n (x:xs) | x == n    = xs 
                   | otherwise = (x: removeFst n xs)
              
srtInts :: [Integer] -> [Integer]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = minl xs

srtInts' :: [Integer] -> [Integer]
srtInts' [] = []
srtInts' xs = let
  m = minl xs
  in m : (srtInts' (removeFst m xs))

average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum' xs) / fromIntegral (length xs)

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length'     [] = 0
length' (x:xs) = 1 + length' xs

count :: Char -> [Char] -> Int
count c [] = 0
count c (x:xs) | c == x    = 1 + count c xs
               | otherwise = count c xs

nrepeat :: Char -> Int -> [Char]
nrepeat _ 0 = []
nrepeat c n = (c : nrepeat c (n-1))

blowup :: [Char] -> [Char]
blowup l = bhelp l 1
  where
    bhelp :: [Char] -> Int -> [Char]
    bhelp [] _ = []
    bhelp (x:xs) n = nrepeat x n ++ bhelp xs (n+1)

srtStr :: [String] -> [String]
srtStr [] = []
srtStr ss = (m : srtStr (dels m ss)) where m = mins ss

mins :: [String] -> String
mins []     = error "empty list"
mins [s]    = s
mins (s:ss) | s < (mins ss) = s
            | otherwise     = (mins ss)

dels :: String -> [String] -> [String]
dels s []                 = []
dels s (x:xs) | s == x    = xs
              | otherwise = x : dels s xs

prefix' :: String -> String -> Bool
prefix' [] _              = True
prefix' _ []              = False
prefix' (s1:ss1) (s2:ss2) | s1 == s2  = prefix ss1 ss2
                         | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring xs (y:ys) = prefix xs ys || substring xs ys
substring _ _        = False

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n

lengths :: [[a]] -> [Int]
lengths xs = map length xs

sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)

primesO :: [Integer] -> [Integer]
primesO xs = filter prime0 [2..]

ldp :: Integer -> Integer
ldp  = ldpf primes1 

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n
        
