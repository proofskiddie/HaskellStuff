
module Parser where

import Prelude hiding (lex, Bool(..))
import Prelude as P (Bool(..))
import Lexer

data Formula a = False
               | True
               | Atom a
               | Not (Formula a)
               | And (Formula a, Formula a)
               | Or  (Formula a, Formula a)
               | Imp (Formula a, Formula a)
               | Iff (Formula a, Formula a)
               | Forall (String, Formula a)
               | Exists (String, Formula a)
{-
data Prop = P String

pname :: Prop -> String
pname (P s) = s

parse_propvar :: [String] -> (Formula Prop, [String])
parse_propvar vs inp = case inp of
  ("(" : _)  -> error "parse_propvar"
  (p : oinp) -> (Atom(P(p)), oinp)
-}

parse_ginfix :: Eq a =>
  a --opsym
  -> ((b -> c) -> b -> b -> c) --opupdate
  -> (b -> c) --sof
  -> ([a] -> (b, [a])) --subparser
  -> [a] --inp
  -> (c, [a])
parse_ginfix opsym opupdate sof subparser inp =
  let (e1, inp1) = subparser inp in
  if inp1 /= [] && head inp1 == opsym then
    parse_ginfix opsym opupdate (opupdate sof e1)  subparser (tail inp1)
  else (sof e1, inp1)


parse_left_infix :: Eq a =>
  a --opsym
  -> ((c,c) -> c) --opcon (sof omitted)
  -> ([a] -> (c, [a])) --subparser
  -> [a] --inp
  -> (c, [a])
parse_left_infix opsym opcon =
  parse_ginfix opsym (\f e1 e2 -> opcon (f e1 ,e2)) id

parse_right_infix :: Eq a =>
  a --opsym
  -> ((c,c) -> c) --opcon (sof omitted)
  -> ([a] -> (c, [a])) --subparser
  -> [a] --inp
  -> (c, [a])
parse_right_infix opsym opcon =
  parse_ginfix opsym (\f e1 e2 -> f (opcon (e1 ,e2))) id


parse_list :: Eq a =>
  a
  -> ([a] -> (b, [a]))
  -> [a]
  -> ([b], [a])
parse_list opsym =
  parse_ginfix opsym (\f e1 e2 -> (f e1 ++ [e2])) (\x -> [x])

papply :: (a -> b) -> (a, c) -> (b, c)
papply f (ast, rest) = (f ast, rest)

nextin :: Eq a => [a] -> a -> P.Bool
nextin inp tok = inp /= [] && head inp == tok

