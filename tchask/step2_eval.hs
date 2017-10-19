#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package parsec
  --package haskeline
  --package containers
-}
import Control.Monad.IO.Class
import Evaluator
import Printer
import Reader
import Readline
import Types

malEval :: Ast -> EvalAst
malEval a = eval a replEnv

malRep :: String -> InputT IO ()
malRep input = do
  let ast' = readStr input
  liftIO $
    putStrLn $
    case ast' of
      (Right ast) ->
        case malEval ast of
          Left err -> prErr err
          Right evaledAst -> prStr evaledAst
      (Left parseError) -> "Parse error on: '" ++ input ++ "': " ++ parseError

main :: IO ()
main = repl malRep
