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

asSymbol :: Ast -> Err Ast
asSymbol a@(ASym _) = Right a
asSymbol a = Left $ UnexpectedType a "symbol"

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
  { outer = Nothing
  , envData = Map.fromList $ fmap (second ANativeFun) builtInFuncs
  }
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
    {unoeifExpected = 3, unoeifActual = AList asts, unoeifForm = "def!"}

getBindings :: MonadError Error m => Ast -> m [Ast]
getBindings bindingExpr =
  case bindingExpr of
    AList bindings -> return bindings
    AVector bindings -> return bindings
    x -> throwError $ UnexpectedType x "list or vector in binding expression"

let' :: EvalM m => [Ast] -> m Ast
let' [bindingExpr, expr] = do
  bindings <- getBindings bindingExpr
  innerEnv <- newEnv
  (boundEnv, _) <- withEnv innerEnv (buildEnv bindings)
  (_, res) <- withEnv boundEnv (eval expr)
  return res
  where
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
    {unoeifExpected = 3, unoeifActual = AList asts, unoeifForm = "let*"}

do' :: EvalM m => [Ast] -> m Ast
do' asts =
  if null asts
    then throwError
           UnexpectedNumberOfElementInForm
           {unoeifExpected = 2, unoeifActual = AList [], unoeifForm = "do"}
    else do
      ress <- mapM eval asts
      return $ last ress

if' :: EvalM m => [Ast] -> m Ast
if' asts =
  case asts of
    [predicate, trueExpr] -> ifImpl predicate trueExpr ANil
    [predicate, trueExpr, falseExpr] -> ifImpl predicate trueExpr falseExpr
    _ ->
      throwError
        UnexpectedNumberOfElementInForm
        {unoeifExpected = 2, unoeifActual = AList asts, unoeifForm = "if"}
  where
    ifImpl predicate trueExpr falseExpr = do
      evaled <- eval predicate
      case evaled of
        ANil -> eval falseExpr
        ABool False -> eval falseExpr
        _ -> eval trueExpr

fn :: EvalM m => [Ast] -> m Ast
fn [bindingExpr, body] = do
  closure <- newEnv
  bindings <- getBindings bindingExpr >>= asSymbols
  return $ AFun $ Function closure $ function bindings
  where
    asSymbols :: MonadError Error m => [Ast] -> m [Ast]
    asSymbols asts = either throwError return $ extract asSymbol asts
    letBinding :: [Ast] -> [Ast] -> Ast
    letBinding bs as = AVector $ concatMap (\(x, y) -> [x, y]) $ zip bs as
    function :: [Ast] -> [Ast] -> EvalAst
    function bindings arguments =
      if length arguments == length bindings
        then Right $ AList [ASym "let*", letBinding bindings arguments, body]
        else Left
               ArgumentCountMismatch
               { acmActualCount = fromIntegral $ length arguments
               , acmArguments = arguments
               , acmExpected = fromIntegral $ length bindings
               }
fn asts =
  throwError
    UnexpectedNumberOfElementInForm
    {unoeifExpected = 2, unoeifActual = AList asts, unoeifForm = "fn*"}

apply :: EvalM m => [Ast] -> m Ast
apply (ASym "def!":xs) = def xs
apply (ASym "let*":xs) = let' xs
apply (ASym "do":xs) = do' xs
apply (ASym "if":xs) = if' xs
apply (ASym "fn*":xs) = fn xs
apply asts = do
  evaled <- evalAst (AList asts)
  case evaled of
    AList xs ->
      case xs of
        ANativeFun func:args -> either throwError return $ func args
        AFun (Function env func):args ->
          case func args of
            Right ast -> fmap snd $ withEnv env $ eval ast
            Left err -> throwError err
        x:_ -> throwError $ UnexpectedElementAtHead x
        [] -> throwError $ UnexpectedElementAtHead $ AList []
    x -> throwError $ UnexpectedElementAtHead x

eval :: EvalM m => Ast -> m Ast
eval (AList []) = return $ AList []
eval (AList asts) = apply asts
eval ast = evalAst ast
