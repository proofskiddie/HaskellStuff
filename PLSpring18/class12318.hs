
data Tree a = Node a (Tree a) (Tree a)
            | Leaf

data TreeIC a b = NodeIC a (TreeIC b a) (TreeIC b a)
                | LeafIC 

-- allows things like this
-- NodeIC 1 (NodeIC "a" LeafIC (NodeIC 2 LeafIC LeafIC)) LeafIC :: TreeIC Int String

mapN :: (happy -> panda) -> [happy] -> [panda]
mapN _ [] = []
mapN f (x:xs) = f x : map f xs

-- can define a fold function for any data type

foldprod :: (a -> b -> c) -> (a, b) -> c
foldprod f (a,b) = f a b

foldeither :: (a -> c) -> (b -> c) -> Either a b -> c
foldeither f _ (Left a)  = f a
foldeither _ f (Right a) = f a

-- Catamorphisms :
-- using fold to build up arbitrary funcitons

-- Parametric polymorphism, first introduced in ML 1975
-- Think of the type of polymorphism offered by java
-- i.e. "generics"
-- c++ also has a similar style (template meta-programming)
-- 



