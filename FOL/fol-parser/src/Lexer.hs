module Lexer where

import Prelude hiding (lex)

matches :: String -> Char -> Bool
matches = flip elem

space :: Char -> Bool
space = matches " \t\n\r"

punctuation :: Char -> Bool
punctuation = matches "()[]{}"

symbolic :: Char -> Bool
symbolic = matches "~!@#$%^&*-+=|\\:;<>.?/"

numeric :: Char -> Bool
numeric = matches "0123456789"

alphanumeric = matches
  "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

lexwhile :: (Char -> Bool) -> String -> (String,String)
lexwhile = span


-- peel off spaces then test the first character left
-- for type and append to list greedly 
lex :: String -> [String]
lex s = case snd $ lexwhile space s of
   [] -> []
   (c:cs) -> (c : fst tokenize) : (lex . snd) tokenize
    where
      tokenize = lexwhile (prop c) cs
      prop c | symbolic c = symbolic
             | alphanumeric c = alphanumeric
             | punctuation c = const False 


