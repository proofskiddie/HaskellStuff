
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
  

  

  
