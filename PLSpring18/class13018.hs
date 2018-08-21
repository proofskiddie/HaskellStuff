-- equational reasoning

-- one of the most basic questions you can ask about a program
-- is it right?

-- why test individual cases when you can show every input satisfies
-- your specification

-- one type of verification in simple type checking

-- how to systematically prove that two programs are the same

-- pure total functional programs

-- double a b = a + b -- pure expression
-- x := currentdata() +  1 -- not pure

-- a total function T1 -> T2 is guarenteed to both terminate
-- and return a value of T2

-- exceptions can be viewed as side effects

-- notions of correctness

-- myline (20 10 3) evaluates to 70
-- forall n, even (n * 2) evalues to True
-- forall f l1 l2 map f (l1 ++ l2) == map f l1 ++ map f l2
-- forall f g l, map f (map g l) == map (f . g) l

-- equality is semantic equality (equal in kind)

-- two programs are equal if they evaluate to the same expression
-- they throw the same exception (not possible for total functions)
-- they loop forever (not possible for total functions)

-- cogruence relation
-- symmetric
-- reflexive
-- transitive
-- respects underlying structure
-- x == y implies f x == f y (why side effects are not allowed)

-- allows substitution

myline m b x = m * x + b
-- Theorem1
-- myline 20 10 3 == 70
-- -----------------------
-- myline 20 10 3 == 20 * 3 + 10
--                == 70

-- evaluation entails equivalence

-- Theorem2
-- forall b x. myline 1 b x == b + x
-- -----------------------
-- myline 1 b x == 1 * b + x
--              == (1 * x) + b
--              == x + b
--              == b + x

-- Theorem3
-- forall m b x. myline m b x == myline x b m
-- -----------------------
-- myline m b x == m * x + b
--              == (x * m) + b
--              == myline x b m

example :: Int -> Int -> Int -> Int
example a b c = a * (b + c)
-- Theorem4
-- forall a b c . 2 * (example a b c) == example (2*a) c b
--------------------------
-- 2 * (example a b c) == 2 * (a * (b + c))
--                     == (2 * a) * (b + c)
--                     == (2 * a) * (c + b)
--                     == example (2*a) c b

-- proof by induction
--
even 0 = True
even 1 = False
even n = even (n - 2)

-- Theorem5
-- forall n . even (n * 2) == True
-- -----------------------
-- by case analysis
-- case n = 0 :  even 0 = True
-- case n = 1 + m, given even m * 2 = True
--   even ((1 + m) * 2) == 
--   even (2 + (m * 2)) ==
--   even (m * 2) == True

-- Theorem5
-- forall f l1 l2 . map f (l1 ++ l2) == map f l1 ++ map f l2
--------------------------
-- by induction on l1
-- base case l1 = [] : map f ([] ++ l2) == map f [] ++ map f l2 == [] ++ map f l2
-- induction given map f (l1 ++ l2) == map f l1 ++ map f l2
-- map f (l : l1 ++ l2 == f l : map f (l1 ++ l2) ==
-- f l : map f l1 ++ map f l2 == map f (l : l1) ++ map f l2








