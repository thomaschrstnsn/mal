module Evaluator where

import Data.Either
import qualified Data.Map.Strict as Map
import Types

asInt :: Ast -> Err Integer
asInt (AInt x) = Right x
asInt a = Left $ UnexpectedType a "integer"

extract :: (Ast -> Err a) -> [Ast] -> Err [a]
extract f xs = do
  let as = fmap f xs
  let errors = lefts as
  case errors of
    [] -> Right $ rights as
    [e] -> Left e
    _ -> Left $ AggregateError errors

extractMap :: (Ast -> Err a) -> Map.Map Key Ast -> Err (Map.Map Key a)
extractMap f m = do
  let tuples = Map.toList m
  values <- extract f $ fmap snd tuples
  return $ Map.fromList $ zip (fmap fst tuples) values

plus :: [Ast] -> EvalAst
plus xs = do
  ints <- extract asInt xs
  return $ AInt $ sum ints

minus :: [Ast] -> EvalAst
minus xs = do
  ints <- extract asInt xs
  let result =
        case ints of
          [] -> 0
          i:is -> i - sum is
  return $ AInt result

multiply :: [Ast] -> EvalAst
multiply xs = do
  ints <- extract asInt xs
  return $ AInt $ product ints

divide :: [Ast] -> EvalAst
divide xs = do
  ints <- extract asInt xs
  _ <-
    if 0 `elem` ints
      then Left DivisionByZero
      else Right ()
  return $ AInt $ foldl1 div ints

replEnv :: Environment
replEnv =
  Map.fromList [("+", plus), ("-", minus), ("*", multiply), ("/", divide)]

evalAst :: Ast -> Environment -> EvalAst
evalAst (ASym sym) env =
  case Map.lookup sym env of
    (Just f) -> Right $ AFun f
    Nothing -> Left $ SymbolNotFound sym
evalAst (AList asts) env = AList <$> extract (`eval` env) asts
evalAst (AVector asts) env = AVector <$> extract (`eval` env) asts
evalAst (AMap m) env = AMap <$> extractMap (`eval` env) m
evalAst x _ = Right x

apply :: Ast -> EvalAst
apply (AList (f:xs)) =
  case f of
    AFun func -> func xs
    _ -> Left $ ExpectedFunctionAtHead f
apply x = Left $ ApplyWhenNoList x

eval :: Ast -> Environment -> EvalAst
eval (AList []) _ = return $ AList []
eval (AList asts) env = do
  evaled <- evalAst (AList asts) env
  apply evaled
eval ast env = evalAst ast env
