{-# LANGUAGE MultiParamTypeClasses #-}

-- more monads

-- how to capture the semantics of an error
-- using monads to handle errors

-- generate rules / semantics for all compound expressions

-- introduce the HASKELL error monad

data ADExp =
  ConstD Float
  | PlusD ADExp ADExp
  | TimesD ADExp ADExp
  | DivideD ADExp ADExp

evalAD :: (Monad m) => ADExp -> m Float
evalAD (ConstD i) = return i
evalAD (PlusD a1 a2) = do
  m <- evalAD a1
  n <- evalAD a2
  return (m + n)
evalAD (TimesD a1 a2) = do
  m <- evalAD a1
  n <- evalAD a2
  return (m * n)
evalAD (DivideD a1 a2) = do
  m <- evalAD a1
  n <- evalAD a2
  if n == 0 then fail "divide by zero"  else return (m / n)


