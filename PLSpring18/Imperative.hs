
-- Defined lambda calculus, compared different eval orders for lambda calc, explored laziness in Haskell
-- Look at how big-step semantics guide the implementation of an interpreter

-- look at how to model state in operational semantics

data A = ValA Int  | Variable String | Add A A | Mul A A deriving Show
data B = ValB Bool | Or B B  | Not B | Eq A A | Lt A A deriving Show

evalA :: A -> Int
evalA (ValA x)     = x
evalA (Add x1 x2) = evalA x1 + evalA x2
evalA (Mul x1 x2) = evalA x1 * evalA x2

evalB :: B -> Bool
evalB (ValB e)   = e
evalB (Or x1 x2) = evalB x1 || evalB x2
evalB (Not e)    = not $ evalB e
evalB (Eq x1 x2) = evalA x1 == evalA x2
evalB (Lt x1 x2) = evalA x1 <  evalA x2

-- how to model state, introduce a function sigma
-- sigma :: Variable Strings -> Integers
-- so big step becomes a three-way relation
-- that now takes a term and a state and produces a value


data S = Ass String A | Skip | Seq S S | IF B S S | While B S

-- the way bool is evaluated the boolean argument is reduced without producing a state
-- note this is unlike C where you can declare variables in the arguments to an if

-- in c or java you would have a global dictionary as the state
-- haskell is pure so that implementation strategy won't work

--
type Store = String -> Int

emptyStore :: Store
emptyStore _ = error "Oh No!"

updateStore :: Store -> String -> Int -> Store
updateStore s id v = \id' -> if id' == id then v else s id'

lookup :: Store -> String -> Int
lookup s = s

evalAS :: Store -> A -> Int
evalAS s (Variable v) = lookup s v
evalAS _ (ValA x) = x
evalAS s (Add x1 x2) = evalAS s x1 + evalAS s x2
evalAS s (Mul x1 x2) = evalAS s x1 * evalAS s x2


class MyMonad m where
  myReturn :: a -> m a
  (>>=)    :: m a -> (a -> m b) -> m b

instance MyMonad Maybe where
  return a = Just a
  Nothing >>= f = Nothing
  Just a  >>= f = f a

