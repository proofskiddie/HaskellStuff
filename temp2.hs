
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len(xs)

len' :: [a] -> Int
len' x = (fst . last) (zip [1..] x)
