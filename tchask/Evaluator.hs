{-# LANGUAGE FlexibleContexts #-}

module Evaluator where

import Control.Arrow (second)
import Control.Monad.Except
import Data.Either
import qualified Data.Map.Strict as Map

import Types

asInt :: Ast -> Err Integer
asInt (AInt x) = return x
asInt a = throwError $ UnexpectedType a "integer"

extract :: (Ast -> Err a) -> [Ast] -> Err [a]
extract f xs = do
  let as = fmap f xs
  let errors = lefts as
  case errors of
    [] -> Right $ rights as
    [e] -> Left e
    _ -> Left $ AggregateError errors

extractMapM :: ErrM m => (Ast -> m a) -> Map.Map Key Ast -> m (Map.Map Key a)
extractMapM f m = do
  let tuples = Map.toList m
  values <- mapM f $ fmap snd tuples
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
  _ <- when (0 `elem` ints) $ throwError DivisionByZero
  return $ AInt $ foldl1 div ints

replEnv :: Environment
replEnv =
  Env
  {outer = Nothing, envData = Map.fromList $ fmap (second AFun) builtInFuncs}
  where
    builtInFuncs = [("+", plus), ("-", minus), ("*", multiply), ("/", divide)]

evalAst :: EvalM m => Ast -> m Ast
evalAst (ASym sym) = getEnv sym
evalAst (AList asts) = do
  xs <- mapM eval asts
  return $ AList xs
evalAst (AVector asts) = do
  xs <- mapM eval asts
  return $ AVector xs
evalAst (AMap m) = AMap <$> extractMapM eval m
evalAst x = return x

def :: EvalM m => [Ast] -> m Ast
def [sym, val] =
  case sym of
    ASym sym' -> do
      evaled <- eval val
      _ <- setEnv sym' evaled
      return evaled
    _ -> throwError $ ExpectedSymbolButFound sym
def asts =
  throwError
    UnexpectedNumberOfElementInForm
    {expected = 3, actual = AList asts, form = "def!"}

let' :: EvalM m => [Ast] -> m Ast
let' [bindingExpr, expr] = do
  bindings <- getBindings
  innerEnv <- newEnv
  (boundEnv, _) <- withEnv innerEnv (buildEnv bindings)
  (_, res) <- withEnv boundEnv (eval expr)
  return res
  where
    getBindings :: MonadError Error m => m [Ast]
    getBindings =
      case bindingExpr of
        AList bindings -> return bindings
        AVector bindings -> return bindings
        x -> throwError $ UnexpectedType x "list or vector in let* binding"
    buildEnv :: EvalM m => [Ast] -> m ()
    buildEnv (ASym sym:val:rest) = do
      evaled <- eval val
      setEnv sym evaled
      buildEnv rest
    buildEnv (notSym:_:_) = throwError $ ExpectedSymbolButFound notSym
    buildEnv [] = return ()
    buildEnv [_] = throwError UnevenNumberOfElementsInLetBinding
let' asts =
  throwError
    UnexpectedNumberOfElementInForm
    {expected = 3, actual = AList asts, form = "let*"}

apply :: EvalM m => [Ast] -> m Ast
apply (ASym "def!":xs) = def xs
apply (ASym "let*":xs) = let' xs
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
