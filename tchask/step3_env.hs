#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package parsec
  --package haskeline
  --package containers
  --package mtl
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Environment as Env
import Evaluator
import Printer
import Reader
import Readline
import Types

newtype AppM a = AppM
  { runAppM :: (ExceptT Error (State Environment)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError Error
             , MonadState Environment
             )

instance EvalM AppM where
  getEnv s = do
    env <- get
    Env.findEnv s env
  setEnv s a = do
    env <- get
    put $ Env.setEnv s a env
  newEnv = do
    env <- get
    return $ Env.newEnv env
  withEnv env expr = do
    orig <- get
    put env
    res <- expr
    next <- get
    put orig
    return (next, res)

instance (MonadError Error AppM) => ErrM AppM

run :: AppM a -> Environment -> (Either Error a, Environment)
run a = runState $ runExceptT (runAppM a)

malEval :: EvalM m => Ast -> m Ast
malEval = eval

readEvalPrint :: String -> Environment -> (String, Environment)
readEvalPrint input env =
  case readStr input of
    (Right ast) ->
      case run (malEval ast) env of
        (Left err, _) -> (prErr err, env)
        (Right evaledAst, env') -> (prStr evaledAst, env')
    (Left parseError) ->
      ("Parse error on: '" ++ input ++ "': " ++ parseError, env)

malRep :: String -> InputT (StateT Environment IO) ()
malRep input = do
  env <- lift get
  let (res, env') = readEvalPrint input env
  lift $ put env'
  liftIO $ putStrLn res

main :: IO ()
main = repl malRep replEnv
