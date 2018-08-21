import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ chars
                char '"'
                return $ String x
    where chars = escaped <|> noneOf "\""
          escaped = char '\\' >>  choice (zipWith escapedChar codes replacements)
          escapedChar code replacement = do { char code ; return replacement }
          codes        = [ 'b',  'n',  'f',  'r',  't', '\\', '\"', '/']
          replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= \ num -> return $ Number (read num)

parseExpr :: Parser LispVal
parseExpr = --  parseAtom
         parseString
         -- <|>  parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)


