{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function
import Prelude hiding (zip, zipWith)
-- Theorems For Free!
-- free facts about polymorphic functions just by looking at their types


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = f x y :  zipWith f xs ys

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- Theorem : For any polymorphic function f with type [a] -> [a], any total function
-- g :: b -> c and a list of [b] we have map g (f l) = f (map g l)


-- key idea behind theorems for free is due to John Reynolds in
-- "Types, Abstraction, and Parametric Polymorphim" developed Logical Relations
-- Phil Wadler noticed an application and wrote 'Theorems for Free' paper

propTheoremMap :: (Eq b, Eq d) => Fun a b -> Fun c d -> [a] -> [c] -> Bool
propTheoremMap (apply -> f) (apply -> g) xs ys =
  (map (\(x, y) -> (f x, g y)) (zip xs ys) == zip (map f xs) (map g ys))
                                                 

-- Define a function that maps types to propositions on values of that type
-- Prove that all such propositions are theorems : Abstraction Theorem
-- Apply the theorem to specific functions                                                 -
