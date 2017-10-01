module Reader
  ( readStr
  ) where

import Text.ParserCombinators.Parsec
import Types

type MalParser a = GenParser Char () a

--ast :: GenParser Char st Ast
ast :: MalParser Ast
ast = do
  result <- expression
  eof
  return result

expression :: MalParser Ast
expression = spaces *> choice [aSym, aList, aInt] <* spaces <?> "expression"

expressions :: MalParser [Ast]
expressions = sepBy expression spaces

aInt :: MalParser Ast
aInt = (AInt . read) <$> many1 digit

aSym :: MalParser Ast
aSym = do
  first <- letter <|> oneOf "+_-*/"
  rest <- many alphaNum
  return $ ASym $ first : rest

aList :: MalParser Ast
aList = do
  _ <- char '('
  exprs <- expressions
  _ <- char ')'
  return $ AList exprs

readStr :: String -> Either String Ast
readStr x = do
  let res = parse ast "(mal stdin)" x
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right r
