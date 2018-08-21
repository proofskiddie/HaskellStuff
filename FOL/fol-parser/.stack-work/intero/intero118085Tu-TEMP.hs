module Main where

import Printer
import Parser
import Text.PrettyPrint

main :: IO Doc
main = do
  line <- getLine
  ret <-if line == "exit" then return empty
        else do
          next <- main
          return $ (print_fol_formula $ parse line) <> text "\n" <> next
  return ret             



