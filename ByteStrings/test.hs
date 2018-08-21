
type Name = String

data Expr
  = Var Name
  | Lit Lit
  | Op PrimOp [Expr]
  | Let Name [Name] Expr

data Lit
  = LitInt Int

data PrimOp
  = Add

