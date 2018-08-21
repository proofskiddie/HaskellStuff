
import Prelude hiding ((/=), (==), Eq)

maxP :: (a -> a -> Bool) -> [a] -> a
maxP cmp [a] = a
maxP cmp (a : as) = let r = (maxP cmp as) in
                    if (cmp a r) then a else r


-- better to be able to use a comparison operator instead of having to pass a function
-- this dosen't work for some reason
maxP' :: Ord a => [a] -> a
maxP' [a] = a
mapP' (a : as) = let r = (maxP' as) in
                 if a < r then a else r


class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y) -- these will only be used to infer the function
                        -- should a definition not be provided
  x /= y = not (x == y)

-- use :info <TypeClassInQuestion> for information about a particular typeclass

data IntList = ConsInt Int IntList | NilInt 
-- 
instance Eq IntList where
  (==) = (\x y -> True)
  (/=) = (\x y -> True)


-- writing a pretty printer

data Aexp =
       Variable String
     | Const Int
     | Plus Aexp Aexp
     | Minus Aexp Aexp

-- class Show a where
--  show :: a -> String

instance Show Aexp where
  show (Variable v) = v -- :: Aexp -> String
  show (Const i) = show i
  show (Plus a1 a2) = show a1 ++ "+" ++ show a2
  show (Minus a1 a2) = show a1 ++ "-" ++ show a2  

-- how would you implement Show for Int??

data PolyAexp v i =
       VariableP v
     | ConstP i
     | PlusP  (PolyAexp v i) (PolyAexp v i)
     | MinusP (PolyAexp v i) (PolyAexp v i)

instance (Show v, Show i) => Show (PolyAexp v i) where
  show (VariableP v) = show v -- :: PolyAexp -> String
  show (ConstP i) = show i
  show (PlusP a1 a2) = show a1 ++ "+" ++ show a2
  show (MinusP a1 a2) = show a1 ++ "-" ++ show a2  


{-
-- this is a type of inheritance
-- you cannot construct an Intergral a unless you already have an
-- instance of Real a and Enum a

class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  {-# MINIMAL quotRem, toInteger #-} -- allows you to define the minimal amount
                                     -- of functions
  	-- Defined in `GHC.Real'
instance Integral Word -- Defined in `GHC.Real'
instance Integral Integer -- Defined in `GHC.Real'
instance Integral Int -- Defined in `GHC.Real'
-}

class YesNo a where
  yesno :: a -> Bool

instance YesNo Bool where
  yesno = id
 
instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Aexp where
  yesno (Const i) = True
  yesno _ = False

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf i t f = if (yesno i) then t else f


--domain specific languages

-- syntax

-- semantics (dynamic semantics)
  -- how do I eval expressions
-- sanity check (static semantics)
  -- what expressions have meaning (ie TYPES)

-- limited expressiveness

-- domain focus

-- examples
-- sql
-- regex
-- make
-- bash

-- dsl pros
-- productivity : focus on details
-- comprehension : clear to non-programmars (analogy)
-- flexibility : non essential details allows optimization

-- dsl cons
-- language cacophony : tower of babel
-- cost : someone has to implement
-- walled garden : dependency on language not used anywhere else
-- impedance mismatch : language abstraction may be out of sync with needs


-- embedded dsls
-- shallow : expressions are programs in host langugage
-- deep : expressions are data types in host language


-- syntax
-- E ::= in F (\x -> E) | where F E | return F
--         | join E E | innerJoin E E on F F | count E
--         | orderBy F E
--   in emp (\x -> where (x.name == "fred") return x.id)

-- semantics ... see the slides

import Data.List
  --syntax
type Relation a = [a]
inT :: Relation a -> (a -> Relation b) -> Relation b
whereT :: Bool -> Relation a -> Relation a
returnT :: a -> Relation a
joinT :: Relation a -> Relation a -> Relation (a, b)

  --semantics (function definitions)

  
