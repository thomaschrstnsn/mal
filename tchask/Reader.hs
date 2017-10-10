{-# LANGUAGE FlexibleContexts #-}

module Reader
  ( readStr
  ) where

import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import Types

type MalParser a = GenParser Char () a

ast :: MalParser Ast
ast = do
  result <- expression
  eof
  return result

expression :: MalParser Ast
expression =
  whitespaces *>
  choice [aComment, aNil, aBool, aSym, aList, aVector, aMap, aInt, aKw, aString] <*
  (whitespaces *> skipMany aComment) <?> "expression"

expressions :: MalParser [Ast]
expressions = sepBy expression whitespaces

aInt :: MalParser Ast
aInt = (AInt . read) <$> many1 digit

aSym :: MalParser Ast
aSym = do
  first <- letter <|> oneOf "+_-*/"
  rest <- many (alphaNum <|> oneOf "-*")
  return $ ASym $ first : rest

aList :: MalParser Ast
aList = AList <$> between (char '(') (char ')') expressions

aVector :: MalParser Ast
aVector = AVector <$> between (char '[') (char ']') expressions

pairs :: [a] -> [(a, a)]
pairs xs = pairs' xs []
  where
    pairs' [] res = reverse res
    pairs' [_] res = reverse res
    pairs' (x:y:xs') res = pairs' xs' ((x, y) : res)

aMap :: MalParser Ast
aMap =
  (AMap . Map.fromList . pairs) <$> between (char '{') (char '}') expressions

aKw :: MalParser Ast
aKw = do
  _ <- char ':'
  keyword <- many1 alphaNum
  return $ AKw keyword

aNil :: MalParser Ast
aNil = ANil <$ string "nil"

aBool :: MalParser Ast
aBool = ABool True <$ string "true" <|> ABool False <$ string "false"

aString :: MalParser Ast
aString = AStr <$> between (char '\"') (char '\"') (many strchar)
  where
    strchar = char '\\' *> escape <|> satisfy (`notElem` "\"\\")

aComment :: MalParser Ast
aComment =
  do _ <- char ';'
     _ <- manyTill anyChar (try newline)
     return AComment
     <?> "comment"

escape :: MalParser Char
escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where
    decode c r = r <$ char c

whitespace :: MalParser Char
whitespace = choice [space, char ','] <?> "whitespace"

whitespaces :: MalParser ()
whitespaces = skipMany whitespace

readStr :: String -> Either String Ast
readStr x = do
  let res = parse ast "(mal stdin)" (x ++ "\n")
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right r
