
module Parser where

import Prelude hiding (lex, Bool(..))
import Lexer

data Formula a = False
               | True
               | Atom a
               | Not (Formula a)
               | And (Formula a) (Formula a)
               | Or  (Formula a) (Formula a)
               | Imp (Formula a) (Formula a)
               | Iff (Formula a) (Formula a)
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

parse_ginfix opsym opupdate sof subparser inp =
  let (e1, inp1) = subparser inp in
  if inp1 /= [] && head inp1 == opsym then
    parse_ginfix opsym opupdate (opupdate (sof e1)) subparser (tail inp1)
  else ((sof e1), inp1)

parse_left_infix opsym opcon =
  parse_ginfix opsym (\f e1 e2 -> opcon (f (e1 ,e2))) (\x -> x)

{-
parse_right_infix opsym opcon =
  parse_ginfix opsym (\f e1 e2 -> f (opcon (e1 ,e2))) (\x -> x)

parse_list opsym =
  parse_ginfix opsym (\f e1 e2 -> (f e1) : [e2]) (\x -> [x])

let rec parse_ginfix opsym opupdate sof subparser inp =
  let e1,inp1 = subparser inp in
  if inp1 <> [] & hd inp1 = opsym then
     parse_ginfix opsym opupdate (opupdate sof e1) subparser (tl inp1)
  else sof e1,inp1;;

let parse_left_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> opcon(f e1,e2)) (fun x -> x);;

let parse_right_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> f(opcon(e1,e2))) (fun x -> x);;

let parse_list opsym =
  parse_ginfix opsym (fun f e1 e2 -> (f e1)@[e2]) (fun x -> [x]);;

-}

