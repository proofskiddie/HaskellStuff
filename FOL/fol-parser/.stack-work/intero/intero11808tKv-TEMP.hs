
module Printer where

import Formula as P
import Text.PrettyPrint

bracket
  :: Show a => Bool -> Int -> (t1 -> t -> a) -> t1 -> t -> Doc
bracket p n f x y = let
  wrap = if p then parens else id in
  nest n $ wrap (text $ show $ f x y)

strip_quant :: Formula t -> ([String], Formula t)
strip_quant fm = case fm of
  Forall(x, yp@(Forall(y,p))) -> let (xs,q) = strip_quant yp in (x:xs,q)
  Exists(x, yp@(Exists(y,p))) -> let (xs,q) = strip_quant yp in (x:xs,q)
  Forall(x,p) -> ([x],p)
  Exists(x,p) -> ([x],p)
  _ -> ([],fm)

print_formula :: Integral a => (a -> b -> Doc) -> Formula b -> Doc
print_formula pfn = pr_formula 0
  where
   pr_formula pr fm = case fm of
                        P.False -> text "false"
                        P.True  -> text "true"
                        Atom(pargs) -> pfn pr pargs
                        Not(p) -> bracket (pr > 10) 1 (print_prefix 10) "~" p
                        And(p,q) -> bracket (pr > 8) 0 (print_infix 8 "/\\") p q
                        Or(p,q) -> bracket (pr > 6) 0 (print_infix 6 "\\/") p q
                        Imp(p,q) -> bracket (pr > 4) 0 (print_infix 4 "==>") p q
                        Iff(p,q) -> bracket (pr > 2) 0 (print_infix 2 "<=>") p q
                        Forall(x,p) -> bracket (pr > 0) 2
                          print_qnt "forall" (strip_quant fm)
                        Exists(x,p) -> bracket (pr > 0) 2
                          print_qnt "exists" (strip_quant fm)
   print_qnt qname (bvs,bod) = 
     text qname <+>
     foldr ((<+>).text) empty bvs <>
     text ". " <>
     nest 0 (pr_formula 0 bod)
   print_prefix newpr sym p =
     text sym <>
     pr_formula (newpr + 1) p
   print_infix newpr sym p q =
     pr_formula (newpr + 1) p <>
     space <> text sym <> space <>
     pr_formula newpr q 
  
print_term prec fm =
  case fm of
    Var x -> text x
    Fn("^",[tm1,tm2]) -> print_infix_term Prelude.True prec 24 "^" tm1 tm2
    Fn("/",[tm1,tm2]) -> print_infix_term Prelude.True prec 22 " /" tm1 tm2
    Fn("*",[tm1,tm2]) -> print_infix_term Prelude.False prec 20 " *" tm1 tm2
    Fn("-",[tm1,tm2]) -> print_infix_term Prelude.True prec 18 " -" tm1 tm2
    Fn("+",[tm1,tm2]) -> print_infix_term Prelude.False prec 16 " +" tm1 tm2
    Fn("::",[tm1,tm2]) -> print_infix_term Prelude.False prec 14 "::" tm1 tm2
    Fn(f,args) -> print_fargs f args

print_fargs f args =
  text f <>
  if args == [] then empty else
   (text "(" <>
   (nest 0 $ print_term 0 (head args)) <>
   (cat $ map (\t -> text ",\n" <> print_term 0 t) (tail args)) <>
    text ")")

sub [] _ _ = []
sub s _ 0 = s
sub (s:ss) 0 m = s : sub ss 0 (m - 1)
sub (s:ss) n m = sub ss (n - 1) (m - 1)

print_infix_term isleft oldprec newprec sym p q =
  if oldprec > newprec then (text "(") else empty <>
  print_term (if isleft then newprec else newprec+1) p <>
  text sym <>
--  print_break (if (sub sym 0 1 == " ") then 1 else 0) 0 <>
  print_term (if isleft then newprec+1 else newprec) q <>
  if oldprec > newprec then (text ")") else empty


{-printert tm =
  open_box 0; text "<<|";
  open_box 0; print_term 0 tm; close_boxempty;
  text "|>>"; close_boxempty;;-}

print_atom prec (R(p,args)) =
  if elem p ["=", "<", "<=", ">", ">="] && length args == 2
  then print_infix_term Prelude.False 12 12 (' ':p) (args!!0) (args!!1)
  else print_fargs p args

print_fol_formula = print_formula print_atom





