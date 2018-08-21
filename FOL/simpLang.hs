import Data.Maybe

data Term = A | B | C | I Term Term deriving (Show, Eq)

initList = [C,B,A]

generate :: Int -> [Term]
generate 0 = initList 
generate n = map (\(x,y) -> I x y) gen ++ (generate 0) where
  gen = [(x,y) | x <- xs, y <- xs]
  xs  = generate (n - 1)

thms :: [Term] -> [Term]
thms [] = []
thms xs = map (\(x,y) -> I x (I y x)) ws where
  ws = [(x,y) | x <- xs, y <- xs]
  
mpMaybe :: [Term] -> [Maybe Term]
mpMaybe [] = []
mpMaybe ys = map (\(x,y) -> 
          case y of
            I a b -> if x == a then Just b else Nothing) ws where
  ws = [(x,y) | x <- ys, y <- ys]
              
mp :: [Term] -> [Term]
mp [] = []
mp xs = catMaybes $ mpMaybe (thms xs)

pretty :: Term -> String
pretty a = case a of
             I a b -> "(" ++ pretty a ++ ">" ++ pretty b ++ ")"
             a -> show a

print xs = map pretty xs
              
main :: IO ()
main = do 
  line <- getLine
  mapM Prelude.print $ Main.print $ mp $ generate (read line :: Int)
  return ()

