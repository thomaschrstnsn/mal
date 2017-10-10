#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package haskeline
-}
import Control.Monad.IO.Class
import Readline

malRead = id

malEval = id

malPrint = putStrLn

malRep :: String -> InputT IO ()
malRep input = liftIO $ (malPrint . malEval . malRead) input

main :: IO ()
main = repl malRep
