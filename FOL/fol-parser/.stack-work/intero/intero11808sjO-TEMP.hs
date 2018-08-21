
module Parser where

import Formula
import Util
import Lexer

import Prelude hiding (lex, Bool(..))
import qualified Prelude as P (Bool(..))
import Control.Exception
import Data.Maybe (isJust, Maybe(..))

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

parse_quant
  :: ([[Char]] -> [[Char]] -> Maybe (Formula a, [[Char]]),
      [[Char]] -> [[Char]] -> (Formula a, [[Char]]))
     -> [[Char]]
     -> (([Char], Formula a) -> Formula a)
     -> [Char]
     -> [[Char]]
     -> (Formula a, [[Char]])
parse_quant (ifn, afn) vs qcon x inp =
  case inp of
    [] -> error "parse_quant"
    (y : rest) -> papply (\fm -> qcon(x, fm))
      (if y == "." then parse_formula (ifn, afn) vs rest
       else parse_quant (ifn, afn) (y : vs) qcon y rest)

parse_formula ::
  ([[Char]] -> [[Char]] -> Maybe (Formula a, [[Char]]),
   [[Char]] -> [[Char]] -> (Formula a, [[Char]]))
  -> [[Char]] -> [[Char]] -> (Formula a, [[Char]])
parse_formula (ifn, afn) vs inp =
  parse_right_infix "<=>" (\(p,q) -> Iff(p,q))
   (parse_right_infix "==>" (\(p,q) -> Imp(p,q))
     (parse_right_infix "\\/" (\(p,q) -> Or(p,q))
       (parse_right_infix "/\\" (\(p,q) -> And(p,q))
         (parse_atomic_formula (ifn, afn) vs )))) inp

is_const_name :: String -> P.Bool
is_const_name s = (isNumeric s) || s == "nil"

parse_atomic_term ::
  Foldable t => t [Char] -> [[Char]] -> (Term, [[Char]])
parse_atomic_term vs inp =
  case inp of
    [] -> error "term expected"
    ("(":rest) -> parse_bracketed (parse_term vs) ")" rest
    ("-":rest) -> papply (\t -> Fn("-",[t])) (parse_atomic_term vs rest)
    (f:"(":")":rest) -> (Fn(f,[]),rest)
    (f:"(":rest) ->
      papply (\args -> Fn(f,args))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
    (a:rest) ->
      ((if is_const_name a && not (elem a vs) then Fn(a,[]) else Var a),rest)

parse_term ::
  Foldable t => t [Char] -> [[Char]] -> (Term, [[Char]])
parse_term vs inp =
  parse_right_infix "::" ( \(e1,e2) -> Fn("::",[e1,e2]))
    (parse_right_infix "+" ( \(e1,e2) -> Fn("+",[e1,e2]))
       (parse_left_infix "-" ( \(e1,e2) -> Fn("-",[e1,e2]))
          (parse_right_infix "*" ( \(e1,e2) -> Fn("*",[e1,e2]))
             (parse_left_infix "/" ( \(e1,e2) -> Fn("/",[e1,e2]))
                (parse_left_infix "^" ( \(e1,e2) -> Fn("^",[e1,e2]))
                   (parse_atomic_term vs)))))) inp

parset :: String -> Term
parset = make_parser (parse_term [])

make_parser :: Eq t => ([String] -> (t1, [t])) -> String -> t1
make_parser pfn s =
  let (expr,rest) = pfn (lex s) in
    if rest == [] then expr else error "Unparsed input"

exists p [] = P.False
exists p (l:ls) | p l       = P.True
                | otherwise = exists p ls


parse_infix_atom vs inp =       
  let (tm,rest) = parse_term vs inp in
  if exists (nextin rest) ["=", "<", "<=", ">", ">="] then                     
        Just $ papply ( \tm' -> Atom(R(head rest,[tm,tm'])))                          
               (parse_term vs (tail rest))                                       
  else Nothing
                                                               
parse_atom vs inp =
  let try_first = parse_infix_atom vs inp in
    if isJust try_first then (\(Just x) -> x) try_first
    else
      case inp of
       (p:"(":")":rest) -> (Atom(R(p,[])), rest)
       (p:"(":rest) ->
          papply ( \args -> Atom(R(p,args)))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
       (p:rest) -> if (p /= "(") then (Atom(R(p,[])),rest) else error "parse_atom"
 

parse = make_parser                                                        
  (parse_formula (parse_infix_atom,parse_atom) [])

