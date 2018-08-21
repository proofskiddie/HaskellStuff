
-- use of maybe as a monad
divBy :: Integral a => a -> [a] -> Maybe [a]
divBy _ [] = return []
divBy _ (0 : _) = fail "division by zero in divBy"
divBy numerator (denom:xs) =
  do next <- divBy numerator xs
     return ((numerator `div` denom) : next)

-- can generalize on monad type without changing function body
divByGeneric :: (Monad m, Integral a) => a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0 : _) = fail "division by zero in divByGeneric"
divByGeneric numerator (denom:xs) =
  do next <- divByGeneric numerator xs
     return ((numerator `div` denom) : next)

--use of either String for error handling
divByEither :: Integral a => a -> [a] -> Either String [a]
divByEither _ [] = Right []
divByEither _ (0:_) = Left "divByEither : division by 0"
divByEither numerator (denom:xs) =
  case divByEither numerator xs of
    Left x -> Left x
    Right results -> Right ((numerator `div` denom) : results)
