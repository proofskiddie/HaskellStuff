

-- two introduction forms
-- are there sub-constructors defined
-- Circle :: Float -> (Float -> (Float -> Shape))
-- intermediate functions 
data Shape = Rectangle Float Float Float Float 
           | Circle Float Float Float
           deriving (Show, Read, Eq)

area :: Shape -> Float
area (Rectangle x1 y1 x2 y2) = (abs (x2 - x1)) * (abs (y2 - y1))
area (Circle _ _ r) = pi * r ^ 2

data Point = Point Float Float deriving (Show)

data Shape' = Rectangle' Point Point | Circle' Point Float deriving (Show)

s = Rectangle' (Point 10.0 10.0) (Point 10.0 10.0)

area' :: Shape' -> Float
area' (Circle' _ r) = pi * r^2
area' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs (x2 - x1)) * (abs (y2 - y1))

-- record types
-- automatically introduce named projection function
data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     , height    :: Float }

-- example of parametric record types

-- how do record types work with typeclasses like deriving show 
data Test a = Test { f :: a }
data Church a = Church { base :: a, successor :: a -> a }

fapply :: Int -> Church a -> a
fapply 0 x = base x
fapply n x = (successor x) (fapply (n-1) x)

-- Record projection functions are global, this will fail
-- data T a = T { base :: a, successor :: a -> a }



append :: [a] -> [a] -> [a]
append (x : xs) ys = x : append xs ys
append _ ys = ys

-- this function will create 2^n recursive calls (bad)
findMax :: [Int] -> Int
findMax [n] = n
findMax (n : l) = if n > findMax l then n else findMax l

-- fixed with memoization (using let binding)
findMax' :: [Int] -> Int
findMax' [n]     = n
findMax' (n : l) = let r = findMax l in
                   if n > r then n else r


