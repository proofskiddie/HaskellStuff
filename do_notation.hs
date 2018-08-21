
main :: IO ()
main = putStr "Hello" >>
       putStr " "     >>
       putStr "world!" >>
       putStr "\n"    >>
     do putStr "Hello"
        putStr " "
        putStr "world!"
        putStr "\n"
        
