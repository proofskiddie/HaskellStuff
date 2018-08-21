
module Formula where

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

--atom_union f fm = 
  
