-- A simple, SQL-like embedded domain specific language
-- It will be convenient to have a lot of list functions
import Data.List


-- Step One: Define the syntax of the language:
-- in tbl1 tbl2 f : "do something for each element of a table"
-- where cond b : "filter the elements of a table"
-- return v : "build the singleton table"
-- join tbl1 tbl2 : "build the product of two tables"
-- innerJoin tbl1 tbl2 : "build the product of two tables on some condition"
-- count tbl : "Count the elements of a table"
-- orderBy : "Sort the elements of a table on some filter"

-- Step Two: What kinds of expressions make sense?
-- One of the key niceties of shallowly embedded DSLs is that
-- we can exploit the type system of the host language to rule out
-- nonsensical expressions

type Relation a = [a]
-- in tbl1 tbl2 f : "do something for each element of a table"
inT :: Relation a -> (a -> Relation b) -> Relation b
-- where cond b : "filter the elements of a table"
whereT :: Bool -> Relation a -> Relation a
-- return v : "build the singleton table"
returnT :: a -> Relation a
-- join tbl1 tbl2 : "build the product of two tables"
joinT :: Relation a -> Relation b -> Relation (a, b)
-- innerJoin tbl1 tbl2 : "build the product of two tables on some condition"
innerJoinT :: Eq c => Relation a -> Relation b
              -> (a -> c) -> (b -> c) -> Relation (a, b)
-- count tbl : "Count the elements of a table"
countT :: Relation a -> Int
-- orderBy : "Sort the elements of a table on some filter"
orderByT :: Ord b => (a -> b) -> Relation a -> Relation a

-- Step Three: Define the semantics of the language:
-- Again, we can leverage the semantics of the host language
-- to define the semantics of our DSL as functions!

-- inT :: [a] -> (a -> [b]) -> [b]
inT l k = foldr (++) [] (map k l)

-- whereT :: Bool -> [a] -> [a]
whereT True a = a
whereT False _ = []

-- returnT :: a -> [a]
returnT a = [a]

-- joinT :: [a] -> [b] -> [(a, b)]
joinT l1 l2 = foldr (\a r -> map (\b -> (a,b)) l2 ++ r) [] l1
-- OR: joinT l1 l2 = inT l1 $ \a -> inT l2 $ \b -> returnT (a, b)

-- innerJoinT :: [a] -> [b] -> [(a, b)]
innerJoinT l1 l2 p1 p2 = inT l1 $ \a -> inT l2
                                $ \b -> whereT  (p1 a == p2 b)
                                $ returnT (a, b)

-- countT :: [a] -> Int
countT = length

-- orderByT :: Ord b => (a -> b) -> [a] -> [a]
orderByT f l = sortBy (\a1 a2 -> compare (f a1) (f a2)) l

main = print $ orderByT id $ inT [1..10]
                           $ \x -> whereT (x > 5)
                           $ returnT (x + 1)
