module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn ("Enter your name: ")
  args <- getLine
  putStrLn ("Hello, " ++ args)
  putStrLn ("Enter first num: ")
  num1 <- getLine
  putStrLn ("Enter second num: ")
  num2 <- getLine
  putStrLn (num1 ++ " + " ++ num2 ++ " = " ++ show (read num1 + read num2))

