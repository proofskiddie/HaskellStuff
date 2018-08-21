
-- defined page 3 of TPL
{-
data T =
    True
  | False
  | If T T T
  | O
  | Succ T
  | Pred T
  | IsZero T
  | ERROR
  deriving (Show, Read)
-}         
data TB =
    BTrue
  | BFalse
  | IfB TB TB TB
  deriving (Show, Read)

-- why in the book TPL is there a seperation into types and values
-- gives multiple declaration error
{-
data TBV =
  BTrue
  BFalse
  deriving (Show, Read)
-}

eval :: TB -> TB
eval (IfB BTrue  t2 t3) = eval t2
eval (IfB BFalse t2 t3) = eval t3
eval (IfB t1 t2 t3) = eval (IfB (eval t1) t2 t3)
eval x = x

-- one step evaluation
eval1 (IfB BTrue t2 t3) = t2  -- E-IfTrue computation rule
eval1 (IfB BFalse t2 t3) = t3 -- E-IfFalse computation rule
eval1 (IfB t t2 t3) = IfB (eval t) t2 t3 -- E-If congruence rule

-- t in E-If has to be of the form IfB _ _ _ by how Haskell pattern matches
-- so the last rule matches rule E-If given on page 34
-- there is no possiblity that eval will throw an exception at line 39

-- but eval1 is not a one-step evaluation rule as
-- eval $ IfB (IfB (IfB BTrue BFalse BFalse) BTrue BFalse) BTrue BFalse
-- evaluates to
-- BFalse

-- proof type for TB
-- corresponds to the derivation tree outlined on page 36
data DerB = E_IfTrue Bool Bool | E_IfFalse Bool Bool | E_If DerB Bool Bool
-- seems like a cool idea but not sure how to build the proofs like this
-- would want a function DerB -> TB


-- going to change the type into values for BTrue, BFalse and Terms for IFB _ _ _



-- page 34 defines the operational sematics of this toy language. Is there a way to
-- encapsulate the possible derivations, using rules E-IfTrue, E-IfFalse and E-If,
-- into a single type ??

-- above, I am trying to create the one step evaluation rule listen on page 34

-- I am having a problem creating a type to go with the grammar on page 34.
-- haskell types are disjoint so there does not seem to be a way to create a
-- value type (true, false) as a part of a term type (true, false, if t then t else t)

-- I tried creating a type
-- data T = IF T T T | V Bool
-- but working with this type does not make it easy to implement
-- the evaluation relations

-- firstly, any such function will be partial

-- E-If has a premis that I am having trouble figuring out how to
-- encode into the argument of a function

-- perhaps using maybe would allow this type to work
-- pattern match on the maybe in the E-If case and return the
-- correct one step result based on whether or not the maybe in Nothing
-- (meaning that the one step rule failed to continue on the input)
-- still the main problem is making a recursive case in E-If and having it
-- halt after one step
-- the guard needs to be checked if it can evaluate once and then that value
-- returned, but cannot be evaluated more than once
-- it still seems just like a simple case match on an IF whose guard is an IF

-- I think the function might actually be total

data Term = IF Term Term Term | V Bool deriving Show

onestep :: Term -> Term
onestep (IF (V True)  t2 t3) = t2
onestep (IF (V False) t2 t3) = t3
onestep (IF (IF t1 t2 t3) t4 t5) = IF (IF (onestep t1) t2 t3) t4 t5

s = IF (V True) (V False) (V False)
t = IF s (V True) (V True)
u = IF (V False) (V True) (V True)

d = onestep (IF t (V False) (V False))

-- I just needed to pattern match on IF for one more depth for it to work
