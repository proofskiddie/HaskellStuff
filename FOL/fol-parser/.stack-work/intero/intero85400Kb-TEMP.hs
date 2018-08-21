
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
   (f:"(":")":rest) -> Fn(f,[]),rest
   (f:"(":rest) ->
      papply (\args -> Fn(f,args))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
  | a::rest ->
      (if is_const_name a & not(mem a vs) then Fn(a,[]) else Var a),rest

and parse_term vs inp =
  parse_right_infix "::" (fun (e1,e2) -> Fn("::",[e1;e2]))
    (parse_right_infix "+" (fun (e1,e2) -> Fn("+",[e1;e2]))
       (parse_left_infix "-" (fun (e1,e2) -> Fn("-",[e1;e2]))
          (parse_right_infix "*" (fun (e1,e2) -> Fn("*",[e1;e2]))
             (parse_left_infix "/" (fun (e1,e2) -> Fn("/",[e1;e2]))
                (parse_left_infix "^" (fun (e1,e2) -> Fn("^",[e1;e2]))
                   (parse_atomic_term vs)))))) inp;;

let parset = make_parser (parse_term []);;

(* ------------------------------------------------------------------------- *)
(* Parsing of formulas.                                                      *)
(* ------------------------------------------------------------------------- *)

let parse_infix_atom vs inp =       
  let tm,rest = parse_term vs inp in
  if exists (nextin rest) ["="; "<"; "<="; ">"; ">="] then                     
        papply (fun tm' -> Atom(R(hd rest,[tm;tm'])))                          
               (parse_term vs (tl rest))                                       
  else failwith "";;
                                                               
let parse_atom vs inp =
  try parse_infix_atom vs inp with Failure _ ->                                
  match inp with                                                               
  | p::"("::")"::rest -> Atom(R(p,[])),rest                                    
  | p::"("::rest ->
      papply (fun args -> Atom(R(p,args)))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
  | p::rest when p <> "(" -> Atom(R(p,[])),rest
  | _ -> failwith "parse_atom";;
                                                                               
let parse = make_parser                                                        
  (parse_formula (parse_infix_atom,parse_atom) []);;              

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

#install_printer print_fol_formula;;

(* ------------------------------------------------------------------------- *)
(* Examples in the main text.                                                *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
<<forall x y. exists z. x < z /\ y < z>>;;

<<~(forall x. P(x)) <=> exists y. ~P(y)>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Semantics, implemented of course for finite domains only.                 *)
(* ------------------------------------------------------------------------- *)

let rec termval (domain,func,pred as m) v tm =
  match tm with
    Var(x) -> apply v x
  | Fn(f,args) -> func f (map (termval m v) args);;

let rec holds (domain,func,pred as m) v fm =
  match fm with
    False -> false
  | True -> true
  | Atom(R(r,args)) -> pred r (map (termval m v) args)
  | Not(p) -> not(holds m v p)
  | And(p,q) -> (holds m v p) & (holds m v q)
  | Or(p,q) -> (holds m v p) or (holds m v q)
  | Imp(p,q) -> not(holds m v p) or (holds m v q)
  | Iff(p,q) -> (holds m v p = holds m v q)
  | Forall(x,p) -> forall (fun a -> holds m ((x |-> a) v) p) domain
  | Exists(x,p) -> exists (fun a -> holds m ((x |-> a) v) p) domain;;

(* ------------------------------------------------------------------------- *)
(* Examples of particular interpretations.                                   *)
(* ------------------------------------------------------------------------- *)

let bool_interp =
  let func f args =
    match (f,args) with
      ("0",[]) -> false
    | ("1",[]) -> true
    | ("+",[x;y]) -> not(x = y)
    | ("*",[x;y]) -> x & y
    | _ -> failwith "uninterpreted function"
  and pred p args =
    match (p,args) with
      ("=",[x;y]) -> x = y
    | _ -> failwith "uninterpreted predicate" in
  ([false; true],func,pred);;

let mod_interp n =
  let func f args =
    match (f,args) with
      ("0",[]) -> 0
    | ("1",[]) -> 1 mod n
    | ("+",[x;y]) -> (x + y) mod n
    | ("*",[x;y]) -> (x * y) mod n
    | _ -> failwith "uninterpreted function"
  and pred p args =
    match (p,args) with
      ("=",[x;y]) -> x = y
    | _ -> failwith "uninterpreted predicate" in
  (0--(n-1),func,pred);;

START_INTERACTIVE;;
holds bool_interp undefined <<forall x. (x = 0) \/ (x = 1)>>;;

holds (mod_interp 2) undefined <<forall x. (x = 0) \/ (x = 1)>>;;

holds (mod_interp 3) undefined <<forall x. (x = 0) \/ (x = 1)>>;;

let fm = <<forall x. ~(x = 0) ==> exists y. x * y = 1>>;;

filter (fun n -> holds (mod_interp n) undefined fm) (1--45);;

holds (mod_interp 3) undefined <<(forall x. x = 0) ==> 1 = 0>>;;
holds (mod_interp 3) undefined <<forall x. x = 0 ==> 1 = 0>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Free variables in terms and formulas.                                     *)
(* ------------------------------------------------------------------------- *)

let rec fvt tm =
  match tm with
    Var x -> [x]
  | Fn(f,args) -> unions (map fvt args);;

let rec var fm =
   match fm with
    False | True -> []
  | Atom(R(p,args)) -> unions (map fvt args)
  | Not(p) -> var p
  | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) -> union (var p) (var q)
  | Forall(x,p) | Exists(x,p) -> insert x (var p);;

let rec fv fm =
  match fm with
    False | True -> []
  | Atom(R(p,args)) -> unions (map fvt args)
  | Not(p) -> fv p
  | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) -> union (fv p) (fv q)
  | Forall(x,p) | Exists(x,p) -> subtract (fv p) [x];;

(* ------------------------------------------------------------------------- *)
(* Universal closure of a formula.                                           *)
(* ------------------------------------------------------------------------- *)

let generalize fm = itlist mk_forall (fv fm) fm;;

(* ------------------------------------------------------------------------- *)
(* Substitution within terms.                                                *)
(* ------------------------------------------------------------------------- *)

let rec tsubst sfn tm =
  match tm with
    Var x -> tryapplyd sfn x tm
  | Fn(f,args) -> Fn(f,map (tsubst sfn) args);;

(* ------------------------------------------------------------------------- *)
(* Variant function and examples.                                            *)
(* ------------------------------------------------------------------------- *)

let rec variant x vars =
  if mem x vars then variant (x^"'") vars else x;;

START_INTERACTIVE;;
variant "x" ["y"; "z"];;

variant "x" ["x"; "y"];;

variant "x" ["x"; "x'"];;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Substitution in formulas, with variable renaming.                         *)
(* ------------------------------------------------------------------------- *)

let rec subst subfn fm =
  match fm with
    False -> False
  | True -> True
  | Atom(R(p,args)) -> Atom(R(p,map (tsubst subfn) args))
  | Not(p) -> Not(subst subfn p)
  | And(p,q) -> And(subst subfn p,subst subfn q)
  | Or(p,q) -> Or(subst subfn p,subst subfn q)
  | Imp(p,q) -> Imp(subst subfn p,subst subfn q)
  | Iff(p,q) -> Iff(subst subfn p,subst subfn q)
  | Forall(x,p) -> substq subfn mk_forall x p
  | Exists(x,p) -> substq subfn mk_exists x p

and substq subfn quant x p =
  let x' = if exists (fun y -> mem x (fvt(tryapplyd subfn y (Var y))))
                     (subtract (fv p) [x])
           then variant x (fv(subst (undefine x subfn) p)) else x in
  quant x' (subst ((x |-> Var x') subfn) p);;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
subst ("y" |=> Var "x") <<forall x. x = y>>;;

subst ("y" |=> Var "x") <<forall x x'. x = y ==> x = x'>>;;
END_INTERACTIVE;;
