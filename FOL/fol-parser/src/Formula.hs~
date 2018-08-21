
module Formula where

import Util

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
               deriving (Eq, Show)

data Term  = Var String | Fn (String, [Term])
data FOL = R (String, [Term]) --aberivation for formulas

instance Functor Formula where
  fmap f (Atom a) = (Atom $ f a)
  fmap f (Not fm) = Not (fmap f fm)
  fmap f (And (fm, fn)) = And ((fmap f fm), (fmap f fn))
  fmap f (Or  (fm, fn)) = Or ((fmap f fm), (fmap f fn))
  fmap f (Imp (fm, fn)) = Imp ((fmap f fm), (fmap f fn))
  fmap f (Iff (fm, fn)) = Iff ((fmap f fm), (fmap f fn))
  fmap f (Forall (str, fm)) = Forall (str, (fmap f fm))
  fmap f (Exists (str, fm)) = Exists (str, (fmap f fm))

instance Foldable Formula where
  foldr f b (Atom a) = (f a b)
  foldr f b (Not fm) = foldr f b fm
  foldr f b (And (fm, fn)) = foldr f (foldr f b fn) fm
  foldr f b (Or  (fm, fn)) = foldr f (foldr f b fn) fm
  foldr f b (Imp (fm, fn)) = foldr f (foldr f b fn) fm
  foldr f b (Iff (fm, fn)) = foldr f (foldr f b fn) fm
  foldr f b (Forall (x, fm)) = foldr f b fm
  foldr f b (Exists (x, fm)) = foldr f b fm

  
{-
data Prop = P String

pname :: Prop -> String
pname (P s) = s

parse_propvar :: [String] -> (Formula Prop, [String])
parse_propvar vs inp = case inp of
  ("(" : _)  -> error "parse_propvar"
  (p : oinp) -> (Atom(P(p)), oinp)
-}

mk_and p q = And (p,q)
mk_or p q = Or (p,q)
mk_imp p q = Imp (p,q)
mk_iff p q = Iff (p,q)
mk_forall x p = Forall(x,p)
mk_exists x p = Exists(x,p)

dest_iff (Iff(p,q)) = (p,q)
dest_iff _ = error "dest_iff"

dest_and (And(p,q)) = (p,q)
dest_and _ = error "dest_and"

conjuncts fm = case fm of
  And (p,q) -> conjuncts p ++ conjuncts q
  _ -> [fm]

dest_or (Or(p,q)) = (p,q)
dest_or _ = error "dest_or"

disjuncts fm = case fm of
  Or (p,q) -> disjuncts p ++ disjuncts q
  _ -> [fm]

dest_imp (Imp(p,q)) = (p,q)
dest_imp _ = error "dest_imp"

antecedent fm = fst (dest_imp fm)
consequent fm = snd (dest_imp fm)

onatoms f fm = fmap f fm

overatoms f fm b = case fm of
  Atom a -> f a b
  Not p -> overatoms f p b
  And(p,q) -> overatoms f p (overatoms f q b)
  Or(p,q) -> overatoms f p (overatoms f q b)
  Imp(p,q) -> overatoms f p (overatoms f q b)
  Iff(p,q) -> overatoms f p (overatoms f q b)
  Forall(x,p) -> overatoms f p b
  Exists (x,p) -> overatoms f p b

onformula f = onatoms ( \(R(p,a)) -> Atom(R(p, map f a)))

--atom_union f fm = 
  
{-
--book example
Fn("sqrt",[Fn("-",[Fn("1",[]);
                   Fn("cos",[Fn("power",[Fn("+",[Var "x"; Var "y"]);
                                        Fn("2",[])])])])]);;

Atom(R("<",[Fn("+",[Var "x"; Var "y"]); Var "z"]));;
-}

is_const_name s = all isNumeric s || s == "nil"


parse_atomic_term vs inp =
  case inp of
    [] -> failwith "term expected"
   ("(":rest) -> parse_bracketed (parse_term vs) ")" rest
   ("-":rest) -> papply (\t -> Fn("-",[t])) (parse_atomic_term vs rest)
   (f:"(":")":rest) -> (Fn(f,[]),rest)
   (f:"(":rest) ->
      papply (\args -> Fn(f,args))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
  | a::rest ->
      ((if is_const_name a & not(mem a vs) then Fn(a,[]) else Var a),rest)

parse_term vs inp =
  parse_right_infix "::" ( \(e1,e2) -> Fn("::",[e1;e2]))
    (parse_right_infix "+" ( \(e1,e2) -> Fn("+",[e1;e2]))
       (parse_left_infix "-" ( \(e1,e2) -> Fn("-",[e1;e2]))
          (parse_right_infix "*" ( \(e1,e2) -> Fn("*",[e1;e2]))
             (parse_left_infix "/" ( \(e1,e2) -> Fn("/",[e1;e2]))
                (parse_left_infix "^" ( \(e1,e2) -> Fn("^",[e1;e2]))
                   (parse_atomic_term vs)))))) inp

parset = make_parser (parse_term [])

parse_infix_atom vs inp =       
  let (tm,rest) = parse_term vs inp in
  if elem (nextin rest) ["=", "<", "<=", ">", ">="] then                     
        papply ( \tm' -> Atom(R(hd rest,[tm,tm'])))                          
               (parse_term vs (tl rest))                                       
  else error ""
                                                               
parse_atom vs inp =
  if isJust (parse_infix_atom vs inp) then error ""
  else case inp of
    (p:"(":")":rest) -> Atom(R(p,[])),rest                                    
    (p:"(":rest) ->
      papply ( \args -> Atom(R(p,args)))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
    (p:rest when p <> "(" -> Atom(R(p,[])),rest
     _ -> error "parse_atom"
                                                                               
parse = make_parser                                                        
  (parse_formula (parse_infix_atom,parse_atom) [])

{-
(* ------------------------------------------------------------------------- *)
(* Set up parsing of quotations.                                             *)
(* ------------------------------------------------------------------------- *)

let default_parser = parse;;

let secondary_parser = parset;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
<<(forall x. x < 2 ==> 2 * x <= 3) \/ false>>;;

<<|2 * x|>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Printing of terms.                                                        *)
(* ------------------------------------------------------------------------- *)

let rec print_term prec fm =
  match fm with
    Var x -> print_string x
  | Fn("^",[tm1;tm2]) -> print_infix_term true prec 24 "^" tm1 tm2
  | Fn("/",[tm1;tm2]) -> print_infix_term true prec 22 " /" tm1 tm2
  | Fn("*",[tm1;tm2]) -> print_infix_term false prec 20 " *" tm1 tm2
  | Fn("-",[tm1;tm2]) -> print_infix_term true prec 18 " -" tm1 tm2
  | Fn("+",[tm1;tm2]) -> print_infix_term false prec 16 " +" tm1 tm2
  | Fn("::",[tm1;tm2]) -> print_infix_term false prec 14 "::" tm1 tm2
  | Fn(f,args) -> print_fargs f args

and print_fargs f args =
  print_string f;
  if args = [] then () else
   (print_string "(";
    open_box 0;
    print_term 0 (hd args); print_break 0 0;
    do_list (fun t -> print_string ","; print_break 0 0; print_term 0 t)
            (tl args);
    close_box();
    print_string ")")

and print_infix_term isleft oldprec newprec sym p q =
  if oldprec > newprec then (print_string "("; open_box 0) else ();
  print_term (if isleft then newprec else newprec+1) p;
  print_string sym;
  print_break (if String.sub sym 0 1 = " " then 1 else 0) 0;
  print_term (if isleft then newprec+1 else newprec) q;
  if oldprec > newprec then (close_box(); print_string ")") else ();;

let printert tm =
  open_box 0; print_string "<<|";
  open_box 0; print_term 0 tm; close_box();
  print_string "|>>"; close_box();;

#install_printer printert;;

(* ------------------------------------------------------------------------- *)
(* Printing of formulas.                                                     *)
(* ------------------------------------------------------------------------- *)

let print_atom prec (R(p,args)) =
  if mem p ["="; "<"; "<="; ">"; ">="] & length args = 2
  then print_infix_term false 12 12 (" "^p) (el 0 args) (el 1 args)
  else print_fargs p args;;

let print_fol_formula = print_qformula print_atom;;
-}


