
module Parser where

import Formula
import Prelude hiding (lex, Bool(..))
import qualified Prelude as P (Bool(..))
import Control.Exception

import Lexer


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

parse_bracketed :: Eq a => (t1 -> (t, [a])) -> a -> t1 -> (t, [a])
parse_bracketed subparser cbra inp =
  let (ast, rest) = subparser inp in
    if nextin rest cbra then (ast, tail rest)
    else error "parse_bracketed"

parse_atomic_formula
  :: ([[Char]] -> [[Char]] -> Maybe (Formula a, [[Char]]),
      [[Char]] -> [[Char]] -> (Formula a, [[Char]]))
     -> [[Char]] -> [[Char]] -> (Formula a, [[Char]])
parse_atomic_formula (ifn, afn) vs inp =
  case inp of
    [] -> error "parse_atomic_formula"
    ("false" : rest) -> (False, rest)
    ("true" : rest) -> (True, rest)
    ("(" : rest) -> (case ifn vs inp of
                      Just x -> x
                      Nothing -> parse_bracketed (parse_formula (ifn, afn) vs) ")" rest)
    ("~" : rest) -> papply (\p -> Not p) (parse_atomic_formula (ifn, afn) vs rest)
    ("forall" : x : rest) ->
      parse_quant (ifn, afn) (x : vs) (\(x,p) -> Forall(x,p)) x rest
    ("exists" : x : rest) ->
      parse_quant (ifn, afn) (x : vs) (\(x,p) -> Exists(x,p)) x rest
    (otherwise) -> afn vs inp

parse_quant (ifn, afn) vs qcon x inp =
  case inp of
    [] -> error "parse_quant"
    (y : rest) -> papply (\fm -> qcon(x, fm))
      (if y == "." then parse_formula (ifn, afn) vs rest
       else parse_quant (ifn, afn) (y : vs) qcon y rest)

parse_formula (ifn, afn) vs inp =
  parse_right_infix "<=>" (\(p,q) -> Iff(p,q))
   (parse_right_infix "==>" (\(p,q) -> Imp(p,q))
     (parse_right_infix "\\/" (\(p,q) -> Or(p,q))
       (parse_right_infix "/\\" (\(p,q) -> And(p,q))
         (parse_atomic_formula (ifn, afn) vs )))) inp



