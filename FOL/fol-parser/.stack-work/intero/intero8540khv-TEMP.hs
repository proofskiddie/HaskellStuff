module Main where

import FOL

repl = do
  line <- getLine
  quick_print $ parse_formula (

main :: IO ()
main = 
