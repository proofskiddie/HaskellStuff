import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

simple :: Parser String
simple = do { char '"'
            ;  words <- many (alphaNum) -- <|> seperator )
            ; char '"'
            ; return ['h']
            }


seperator = do { try $ string "\\\"" ; return $ '"'}

run :: Show a => Parser a -> String -> IO ()
run p input
       = case (parse p "" input) of
           Left err -> do { putStr "parse error at"
                          ; print err
                          }
           Right x -> print x

main :: IO ()
main = do
       line <- getLine
       run simple line
