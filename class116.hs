

intEx :: Int
intEx = 12

funStringCharex :: String -> Char
funStringCharex [] = 'e'
funStringCharex (x : xs) = x

boolBoolBoolex :: Bool -> Bool -> Bool
boolBoolBoolex _ _ = True

type Foo = (Int, String, Int)    -- actual ((Int, String), Int)
type Foo2 = (Int, (String, Int))
  -- does this so that currying works, tuples are left associative
  -- function arrows are right associative

type FooSum = Either Int Bool  
  -- Either introduction rules :
     --  A -> Either A B
     --  B -> Either A B

-- often want to define new datatypes by their introduction and elimination forms
data TwoNumbers = SimpleNum Int | DoubleNum Int Int -- is this an Either ?? 


data IntList = EmptyList | Cons Int IntList