module Evaluator where

import Control.Arrow (second)
import Control.Monad.Except
import Data.Either
import qualified Data.Map.Strict as Map

-- import Environment as Env
import Types

asInt :: Ast -> Err Integer
asInt (AInt x) = return x
asInt a = throwError $ UnexpectedType a "integer"

extract :: ErrM m => (Ast -> Err a) -> [Ast] -> m [a]
extract f xs = do
  let as = fmap f xs
  let errors = lefts as
  case errors of
    [] -> return $ rights as
    [e] -> throwError e
    _ -> throwError $ AggregateError errors

extractMap :: ErrM m => (Ast -> m a) -> Map.Map Key Ast -> m (Map.Map Key a)
extractMap f m = do
  let tuples = Map.toList m
  values <- mapM f $ fmap snd tuples
  return $ Map.fromList $ zip (fmap fst tuples) values

plus :: ErrM m => [Ast] -> m Ast
plus xs = do
  ints <- extract asInt xs
  return $ AInt $ sum ints

minus :: ErrM m => [Ast] -> m Ast
minus xs = do
  ints <- extract asInt xs
  let result =
        case ints of
          [] -> 0
          i:is -> i - sum is
  return $ AInt result

multiply :: ErrM m => [Ast] -> m Ast
multiply xs = do
  ints <- extract asInt xs
  return $ AInt $ product ints

divide :: ErrM m => [Ast] -> m Ast
divide xs = do
  ints <- extract asInt xs
  _ <- when (0 `elem` ints) $ throwError DivisionByZero
  return $ AInt $ foldl1 div ints

replEnv :: Environment
replEnv =
  Env
  {outer = Nothing, envData = Map.fromList $ fmap (second AFun) builtInFuncs}
  where
    builtInFuncs = [] -- [("+", plus), ("-", minus), ("*", multiply), ("/", divide)]

evalAst :: EvalM m => Ast -> m Ast
evalAst (ASym sym) = getEnv sym
evalAst (AList asts) = do
  xs <- mapM eval asts
  return $ AList xs
evalAst (AVector asts) = do
  xs <- mapM eval asts
  return $ AVector xs
evalAst (AMap m) = AMap <$> extractMap eval m
evalAst x = return x

def :: EvalM m => [Ast] -> m Ast
def [sym, val] =
  case sym of
    ASym sym' -> do
      _ <- setEnv sym' val
      return AVoid
    _ -> throwError $ ExpectedSymbolButFound sym
def asts =
  throwError UnexpectedNumberOfElementInForm {expected = 3, actual = AList asts}

apply :: EvalM m => [Ast] -> m Ast
apply (ASym "def!":xs) = def xs
apply asts = do
  evaled <- evalAst (AList asts)
  case evaled of
    AList xs ->
      case xs of
        AFun func:args -> either throwError return $ func args
        x:_ -> throwError $ UnexpectedElementAtHead x
        [] -> throwError $ UnexpectedElementAtHead $ AList []
    x -> throwError $ UnexpectedElementAtHead x

eval :: EvalM m => Ast -> m Ast
eval (AList []) = return $ AList []
eval (AList asts) = apply asts
eval ast = evalAst ast
