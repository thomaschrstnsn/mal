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
  choice
    [ aComment
    , try aNil
    , try aBool
    , try aInt
    , aSym
    , aList
    , aVector
    , aMap
    , aKw
    , aString
    ] <*
  (whitespaces *> skipMany aComment) <?> "expression"

expressions :: MalParser [Ast]
expressions = sepBy expression whitespaces

astAsKey :: Ast -> Key
astAsKey (AKw kw) = KKw kw
astAsKey (ABool b) = KBool b
astAsKey (AInt i) = KInt i
astAsKey (AStr s) = KStr s
astAsKey _ = error "Ast to key translation for unknown"

key :: MalParser Key
key =
  whitespaces *> (astAsKey <$> choice [aBool, aInt, aKw, aString]) <*
  (whitespaces *> skipMany aComment) <?> "key"

keyValuePairs :: MalParser [(Key, Ast)]
keyValuePairs = sepBy keyValuePair whitespaces

keyValuePair :: MalParser (Key, Ast)
keyValuePair = (,) <$> key <*> expression

readNeg :: String -> Integer
readNeg ('-':ds) = negate $ read ds
readNeg x = read x

aInt :: MalParser Ast
aInt = AInt . readNeg <$> (many1 digit <|> negNumber)

negNumber :: MalParser String
negNumber = do
  first <- char '-'
  rest <- many1 digit
  return $ first : rest

aSym :: MalParser Ast
aSym = do
  first <- letter <|> oneOf "+_-*/"
  rest <- many (alphaNum <|> oneOf "-*!")
  return $ ASym $ first : rest

aList :: MalParser Ast
aList = AList <$> between (char '(') (char ')') expressions

aVector :: MalParser Ast
aVector = AVector <$> between (char '[') (char ']') expressions

aMap :: MalParser Ast
aMap = (AMap . Map.fromList) <$> between (char '{') (char '}') keyValuePairs

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
     return AVoid
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
