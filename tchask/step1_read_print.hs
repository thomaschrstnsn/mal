#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package parsec
  --package haskeline
  --package containers
-}
import Control.Monad.IO.Class
import Printer
import Reader
import Readline

malEval :: a -> a
malEval = id

malRep :: String -> InputT IO ()
malRep input = do
  let ast' = readStr input
  liftIO $
    putStrLn $
    case ast' of
      (Right ast) -> (prStr . malEval) ast
      (Left parseError) -> "Parse error on: '" ++ input ++ "': " ++ parseError

main :: IO ()
main = repl malRep
