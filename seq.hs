
data Node = ONE | TWO | ZERO deriving (Show, Eq)
a :: Int -> [Node]
a 0 = [ZERO]
a 1 = a 0 ++ [ONE]
a 2 = a 1 ++ [TWO]
a n = a (n `mod` 3) ++ a (n - 1)

