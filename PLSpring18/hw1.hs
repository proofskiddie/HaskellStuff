-- CS 456 Homework 1
-- Due date: 02/02/2018 by 9:00PM

-- Name: Steven Kidd
-- Email: kidd9@purdue.edu

-- ASSIGNMENT INSTRUCTIONS:
-- ========================================================================
-- All questions have to be completed. Before submitting, make sure
-- that running 'ghc hw1.hs' in the shell completes without errors.
-- If your homework does not compile, it will not be graded! If an
-- incomplete answer to a question is causing ghc to fail, comment it
-- out.

-- SUBMISSION INSTRUCTIONS:
-- =========================================================================
-- Submit all of your files via turnin using the instructions here (windows
-- users will perform a similar incantation):
-- https://www.cs.purdue.edu/homes/cs177/turnin-from-home-macos.html
-- Use cs456 for the class name and hw1 for the project name:
-- ssh <your username>@lore.cs.purdue.edu /usr/local/bin/turnin -c cs456 -p hw1 <your files>


-- Part 1: Haskell Basics
-- For this portion of the homework, you'll be writing haskell
-- functions to help folks train for a triathlon. (Don't worry, you
-- won't be asked to do any training yourself. )

-- A RunRecord is a haskell value of the type (String, Float, Int),
-- where the first entry is the run's location, the second entry is
-- the total run distance in meters, and the third entry is the total
-- run time in seconds.

type RunRecord = (String, Float, Float)

--Question 1: Write a function [pace] that calculates the average pace of a run in m/s.
pace :: RunRecord -> Float
pace (_, dist, time) = dist / time

-- Question 2: Write a function [comparePace] which takes two
-- RunRecords and returns the RunRecord with the better pace

comparePace :: RunRecord -> RunRecord -> RunRecord
comparePace r1 r2 | pace r1 > pace r2 = r1
                  | otherwise         = r2
                      
-- Question 3: Write a function [bestPace] that takes a list of
-- RunRecords and a location (String) and returns the best pace for
-- that location. You can assume that the pace is never less than 0.
bestPace :: [RunRecord] -> String -> Float
bestPace r s = pace $ foldr comparePace (head r) filtr where
               filtr = filter (\ (s1, _, _) -> s1 == s) r

-- Question 4: Write a function that takes a list of RunRecords and a
-- list of locations and returns a list of pairs of (location, Float)
-- holding the location and best pace for each location in the list
bestPaces :: [RunRecord] -> [String] -> [(String, Float)]
bestPaces r = map (\s -> (s, bestPace r s))

---Question 5: I would like to know how much more practice runs I need
--before I hit my target pace for an upcoming triathlon (it's probably
--a lot). Write a function that takes a list of my previous runs in
--chronological order, and a target pace, and calculates that number
--of runs it will take. (hint: use the list of previous runs to
--calculate average improvement on a per-race basis.)

howManyMore :: [RunRecord] -> Float -> Int
howManyMore [] _  = error "howManyMore : empty record"
howManyMore [x] t = if (pace x == t) then 0 else error "howManyMore : not enough data"
howManyMore rs t  = let rem = ceiling ((t - lastPace) / avgImprovement)
                    in if (rem > 0) then rem else 0
  where
    avg =(lastPace - firstPace) / (fromIntegral $ div (length rs) 2)
    avgImprovement = if (avg > 0) then avg else 0
    firstPace = pace . head $ rs
    lastPace  = pace . last $ rs



-- Part 2: Ill-Typed Expressions
-- Explain why each of the following programs fail to type check by in
-- the strings explain[6-8], and change the code in some small way so
-- that they do. problem[6-8] should be uncommented in the file you
-- submit. Note: Don't change the type of the expression!

explain6 :: String
explain6 = "The function (==) has type (a -> a -> Bool). \
            \ Both arguments are expected to have the same type, \
            \ but the arguments passed have different types."

problem6 :: Bool
problem6 = 'a' == 'a'

explain7 :: String
explain7 = "the function comparepace is passed a product type. \
            \ functions in haskell are curried by defulat. \
            \ The type of comparePace is (RunRecord -> RunRecord -> RunRecord), \
            \ which expects a single RunRecord as its first argument. \
            \ This is a type mismatch error"
            

problem7 :: RunRecord
problem7 = comparePace ("Chicago", 1000, 360) ("Austin", 2000, 240)

explain8 :: String
explain8 = "The type produced by problem8 is ((Int, Int), Int) which is a \ 
            \ different type then the expected (Int, Int, Int) provided in the \
            \ function signature. \
            \ (,) :: a -> b -> (a, b) and (,,) :: a -> b -> c -> (a, b, c). \
            \ Haskell types do not overlap, so (a,b,c) cannot be a subtype of \
            \ (a,b), as implied by the function."


problem8 :: (Int, Int, Int)
problem8 = tripleIt 1
  where tripleIt n = let p1 = (n, n, n) in p1

-- Part 3: Inductive Data Structures and recursion
-- Here is the [IntTree] data type from class; in the next three
-- questions, show how to encode the given trees using this data type.

data IntTree =
     Node Int IntTree IntTree
     | Leaf deriving (Show)

singleton :: Int -> IntTree
singleton n = Node n Leaf Leaf

-- Question 9.
--         2
--        / \
--       /   \
--      /     \
--     1       3
--            / \
--           9   1
question9 :: IntTree
question9 = Node 2 (singleton 1) (Node 3 (singleton 9) (singleton 1))
                

-- Question 10.
--      10
--      / \
--     5   8
--        /
--       2
question10 :: IntTree
question10 = Node 10 (singleton 5) (Node 8 (singleton 2) Leaf)

-- Question 11.
--         2
--        / \
--       /   \
--      /     \
--     1       3
--    / \     / \
--   0   7   9   1
--  /   / \     / \
-- 2   1   0   8   8
--        /
--       7
question11 :: IntTree
question11 = Node 2
               (Node 1
                 (Node 0
                   (singleton 2)
                   Leaf)
                 (Node 7
                   (singleton 1)
                   (Node 0
                     (singleton 7)
                     Leaf)))
               (Node 3
                 (singleton 9)
                 (Node 1
                   (singleton 8)
                   (singleton 8)))

-- Question 12. Write a membership function that checks if an integer
-- is in an IntTee (note that the tree need not be balanced).

membership :: IntTree -> Int -> Bool
membership (Node x tl tr) i | x == i    = True
                            | otherwise = membership tl i || membership tr i
membership _ i = False                            


-- Part 4. Polymorphism and higher-order functions.

-- The Maybe type (also called option) is commonly used to represent
-- errors or exceptional cases in functions without throwing an
-- runtime error. The type has two constructors:
-- data Maybe a = Just a | Nothing
-- Think of the Just constructor as representing a normal
-- value of type a, while None as representing "undefined case". As an
-- example, consider this division function:
division :: Float -> Float -> Maybe Float
division n m = if m == 0 then Nothing else Just (n / m)

-- Question 13. Write the getNth function, which returns the nth
-- element of a list or None if the index would go beyond the end of a
-- list.
-- getNth [2, 4, 6, 8] 3 = Just 8
-- getNth [2, 4, 6, 8] 4 = Nothing

getNth :: [a] -> Int -> Maybe a
getNth [] _ = Nothing
getNth (x:xs) i | i <  0    = Nothing
                | i == 0    = Just x
                | otherwise = getNth xs (i - 1)

-- A function calling division or getNth can thus "catch" errors by
-- checking the value of the result using a case expression. This
-- pattern is so prevalent that we may want to write a specialized
-- catch function to capture it.

-- Question 14. Write such a catch function. Here are some example outputs:
-- catch Nothing (\x. x + x) 0 = 0
-- catch (Just 3) (\x. x + x) 0 = 6
-- catch (Just 15) (\x. x + x) 0 = 30

catch :: Maybe a -> (a -> b) -> b -> b
catch m f z = case m of
                Just x  -> f x
                Nothing -> z

-- The Maybe type is also quite useful for representing partial
-- functions (like division).

-- Question 15.
-- Write a specialized polymorphic map function that takes such a
-- partial function and maps it over the elements of a list, dropping
-- elements on which the function is undefined.

-- Examples:
-- mapPartial (division 2) [6, 5, 8, 0, 10] = [3, 2.5, 4, 5]
-- mapPartial (getNth ["a", "b", "c", "d", "e", "f"]) [0, 10, 3, 0] = ["a", "d", "a"]

mapPartial :: (a -> Maybe b) -> [a] -> [b]
mapPartial f [] = []
mapPartial f (x:xs) = case f x of Just e -> e : recurse ; Nothing -> recurse
  where
    recurse = mapPartial f xs
                      
-- Question 16.
-- Recall that a function applied to a single tuple of arguments is equivalent to a function
-- that takes an argument and returns a function that takes the second argument. Write
-- curry and uncurry functions that performs this conversion:

curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x,y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x,y) -> f x y
  
-- Part 5
-- In this portion of the homework, you will implement a simple
-- pattern matching language, akin to how one would implement the case
-- expressions in Haskell:

-- We begin with inductive datatypes whose values represent the
-- patterns and the corresponding values they match against.

data Pattern =
     WildCardP
     | VariableP String
     | UnitP
     | ConstP Int
     | TupleP [Pattern]
     | ConstructorP String Pattern
     deriving Show

data Value =
     ConstV Int
     | UnitV
     | TupleV [Value]
     | ConstructorV String Value
     deriving Show
-- Given v :: Value and p :: Pattern, either p matches v or not. If it does, the match produces
-- a list of (String, Value) pairs showing how values in v are 'bound' to pattern variables in p;
-- order in the list does not matter. The rules for matching should be unsurprising:
-- • Wildcard{ matches everything and produces the empty list of bindings.
-- • VariableP s matches any value v and produces the one-element list holding (s,v).
-- • UnitP matches only Unit and produces the empty list of bindings.
-- • ConstP 17 matches only ConstV 17 and produces the empty list of bindings (and similarly for other integers).
-- • TupleP ps matches a value of the form TupleV vs if ps and vs have the same length and for all i, the ith
--   element of ps matches the ith element of vs. The list of bindings produced is all the lists from the
--   nested pattern matches appended together.
-- • ConstructorP s1 p matches ConstructorV s2 v if s1 and s2 are the same String (you can compare them with ==)
--   and p matches v. The list of bindings produced is the list from the nested pattern match. We call the strings
--   s1 and s2 the 'constructor name'.
-- • Nothing else matches.

-- Question 17. g is an intentially opaquely-named function that does
-- something with patterns. In String 17, give g's type
-- signature and describe in a few sentences what arguments g takes
-- and what g computes.

g f1 f2 WildCardP = f1
g f1 f2 (VariableP var) = f2 var
g f1 f2 (ConstructorP name pat) = g f1 f2 pat
g f1 f2 (TupleP pats) = foldl (\ n p -> g f1 f2 p + n) 0 pats
g _ _ _ = 0

explain17 :: String
explain17 = ".\
\ 'g f1 f2 WildCardP = f1' implies that g returns an element with the same type \n \
\ as f1. By 'g _ _ _' = 0 we know this type is Int (or any Num instance). \n \
\ 'VariableP :: String -> Pattern' along with \n \
\ 'g f1 f2 (VariableP var) = f2 var imply 'f2 :: String -> Int'.  \n \
\ So 'g :: Num a => a -> (String -> a) -> Pattern -> a'. \n \
\ \n \
\ Pattern has WildCardP, VariableP, ConstP and UnitP as its base constructors.\n \
\ These can be seen as leaves on any tree representation of an element in Pattern.\n \
\ g is a function which takes the leaves of this tree, convernts them to \n \
\ some Num type according to f1 and f2 and then sums all the values together. \n \
\ Argument f1 determines what value is substituted for WildCardP, f2 for VariableP. \n \
\ Where 'f2 :: Num a => String -> a' maps variable names to some Num type.\n \
\ Both UnitP and ConstP leaves are substituted for 0. \n \
\"

             

-- Question 18. Use g to define a function countWildCards that takes a
-- pattern and returns how many Wildcard patterns it contains.
countWildCards :: Pattern -> Int
countWildCards = g 1 (const 0) 

-- Question 19. Use g to define a function
-- countWildCardsAndVariableLengths that takes a pattern and returns
-- the number of Wildcard patterns it contains plus the sum of the
-- String lengths of all the variables in the variable patterns it
-- contains.  We care only about variable names; the
-- constructor names are not relevant.
countWildCardsAndVariableLengths :: Pattern -> Int
countWildCardsAndVariableLengths = g 1 length

-- Question 20. Use g to define a function countSomeVar that takes a
-- String and a pattern and returns the number of times the string
-- appears as a variable in the pattern. We care only about variable
-- names; the constructor names are not relevant.
countSomeVar :: String -> Pattern -> Int
countSomeVar s = g 0 $ (\x -> if x then 1 else 0) . (s ==)

-- Question 21. Write a function checkPat that takes a pattern and
-- returns true if and only if all the variables appearing in the
-- pattern are distinct from each other (i.e., use different
-- strings). The constructor names are not relevant. Hints: The sample
-- solution uses two helper functions. The first takes a pattern and
-- returns a list of all the strings it uses for variables. Using
-- foldl with a function that uses append is useful in one case. The
-- second takes a list of strings and decides if it has
-- repeats. Sample solution is ~15 lines.

checkPat :: Pattern -> Bool
checkPat pat =
  (g 0 (\x -> if (countSomeVar x pat) == 1 then 1 else 0) pat) == (g 0 (const 1) pat)

    

-- Question 22. Write a function match that takes a Value and a
-- Pattern and returns a Maybe [(String, Value)], returning Nothing if
-- the pattern does not match and Just lst where lst is the list of
-- bindings if it does. Note that if the value matches but the pattern
-- has no patterns of the form Variable s, then the result is Just
-- []. Hints: Sample solution has one case expression with 7
-- branches. Our sample solution is ~15 lines. Remember to look above
-- for the rules for what patterns match what values, and what
-- bindings they produce.

match :: Value -> Pattern -> Maybe [(String, Value)]
match val (VariableP var)     = return [(var, val)]
match (ConstV n) (ConstP m)   = if (n == m) then return [] else Nothing
match UnitV UnitP             = return []
match (TupleV []) (TupleP []) = return []
match (TupleV (v:vs)) (TupleP (p:ps)) = do
  mh <- match v p
  mt <- match (TupleV vs) (TupleP ps)
  return $ mh ++ mt
match (ConstructorV _ val) (ConstructorP _ pat) = match val pat
match _ WildCardP = return []
match _ _ = Nothing


-- Question 23. Write a function firstMatch that takes a value and a
-- list of patterns and returns a Maybe [(String, Value)], namely
-- Nothing if no pattern in the list matches or Just lst where lst is
-- the list of bindings for the first pattern in the list that
-- matches. Hint: Your answers from part 4 may be useful here.

firstMatch :: Value -> [Pattern] -> Maybe [(String, Value)]
firstMatch val (x : xs) = case match val x of
  Nothing -> firstMatch val xs
  Just l  -> Just l
  
main = print $ pace ("Yo", 2, 3)
