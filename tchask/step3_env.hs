#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package parsec
  --package haskeline
  --package containers
  --package mtl
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import App (run)
import Control.Monad.Except
import Control.Monad.State.Strict
import Evaluator
import Printer
import Reader
import Readline
import Types

readEvalPrint :: String -> Environment -> (String, Environment)
readEvalPrint input env =
  case readStr input of
    (Right ast) ->
      case run (eval ast) env of
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
