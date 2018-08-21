import Prelude (String, Int, Read, Show)
-- difference between abstract and concrete syntax
-- concrete syntax : actual program
-- abstract syntax : AST

-- how to remove ambiguity from an AST

-- a grammar :
-- P ::= Val | () | Int | (P,..,P) | C P | *

-- an expression :
-- Cons (x, Cons 2 (Cons * Nil ()))


-- an example of a deeply embedded dsl
-- embedded the AST as an inductive data type inside the language
data Pattern = WildCardP | VariableP String | UnitP | ConstP Int
             | TupleP [Pattern] | ConstructorP String Pattern

-- lambda calculus
-- captures the core feature of any programming language, namely abstraction

-- t ::= x | \ x . t | t t
-- expression : \x . \y . x y x

-- application is left associative s t u == (s t) u
-- abstractions bind as far to the left as possible
-- \ x . y ( \ z . x )

data Expr = Var String | App Expr Expr | Lam String Expr deriving (Show, Read)

-- simple Imp language
-- represents imperative languages

-- a := Val | Int | - a | a opa a
-- b := true | false | not b | b opb b | a opr a
-- opa ::= + | - | * | /
-- opb ::= and | or
-- opr ::= > | <
-- S ::= val := a | skip | S ; S | if b then S else S | while b do S


-- is there a way to close the scope of all the other datatypes into S
data Opa = Add | Sub | Mul | Div
  deriving Show
data Opb = And | Or
  deriving Show
data Opr = Gt | Lt
  deriving Show
data AExpr = AVar String | Val Int | N AExpr | AOp Opa AExpr AExpr
  deriving Show
data BExpr = True | False | Not BExpr | Bop Opb BExpr | Rop Opr AExpr
  deriving Show
data S = Eq String AExpr | Skip | Seq S S | IF BExpr S S | While BExpr S
  deriving Show


expression1 = Seq (Seq (Eq "x" (Val 1)) (Eq "y" (Val 2))) (While True (Eq "x"
  (AOp Add (AVar "x") (AVar "y"))))


-- lexical scoping : every variable identifier refers to the closest (for some def) definition

-- a term with no free variables is called a combinator

-- closures solve the problem of evaluating functions with free varibles in different contexts

-- I bet I could implement closures with a Reader monad.


-- dynamic scoping

-- use the environment when the function is called (not defind)
-- will look up most recent definitions regardless of whether or not
-- function was defined with values

-- seems like dynamic scoping is the same as implicit argument passing or
-- could at least be implemented with implicit arguments.

-- with dynamic scoping you can change the types of terms if the language allows
-- no way of knowing what the type of free variables will be statically

-- dynamic scoping is more efficient be cause you do not have to save contexts
-- but it is possible to statically remove closures to use lexical scoping






