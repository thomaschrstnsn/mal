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
import Control.Monad.IO.Class
import Evaluator
import Printer
import Reader
import Readline

malRep :: Repl ()
malRep input = do
  let ast' = readStr input
  liftIO $
    putStrLn $
    case ast' of
      (Right ast) ->
        case run (eval ast) replEnv of
          (Left err, _) -> prErr err
          (Right evaledAst, _) -> prStr evaledAst
      (Left parseError) -> "Parse error on: '" ++ input ++ "': " ++ parseError

main :: IO ()
main = repl malRep ()
