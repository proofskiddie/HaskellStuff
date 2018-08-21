
data Term = Var String | Fn (String, [Term]) deriving (Show,Eq)
data Fol = R (String, [Term])
