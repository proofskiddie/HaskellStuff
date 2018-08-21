
paren_recursive :: Int -> [String]
paren_recursive 1 = ["()"]
paren_recursive n = -- map (\x -> ("()" ++ x)) (paren_recursive (n - 1)) ++
                    map (\x -> ("(" ++ x ++ ")")) (paren_recursive (n - 1))
                    ++ [(last $ paren_recursive (n - 1)) ++ "()"]
