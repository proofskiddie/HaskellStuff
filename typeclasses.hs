
import Prelude hiding (Functor, fmap)
import Data.List

data Tree a =
   Leaf a
 | Branch (Tree a) (Tree a) deriving Show

-- this constrains a type f
class Functor f where
  fmap       :: (a -> b) -> f a -> f b

instance Functor Tree where
  fmap f (Leaf x)       = Leaf   (f x)
  fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)

-- haskell has two type systems, one checks that types are used correctly

plustree :: Num a  => (a -> a) -> Tree a -> Tree a
plustree f t = fmap f t

data Color = Red | Green | Blue | Indigo | Violet deriving (Eq, Show, Read, Enum)
            
readsTree          :: (Read a) => ReadS (Tree a)
readsTree ('<':s)  = [(Branch l r, u) | (l, '|':t) <- readsTree s,
                                        (r, '>':u) <- readsTree t ]
readsTree s        = [(Leaf x, t)     | (x,t)      <- reads s]

