
module Lib where


non :: (a -> Bool) -> a -> Bool
non p x = not (p x)

check p x = if p x then x else error "check"

funpow n f x | n < 1     = x
             | otherwise = funpow (n - 1) f (f x)

--repeat f x = 
