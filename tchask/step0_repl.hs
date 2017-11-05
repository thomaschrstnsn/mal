#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package haskeline
  --package mtl
-}
import Control.Monad.IO.Class
import Readline

malRead = id

malEval = id

malPrint = putStrLn

malRep :: Repl ()
malRep input = liftIO $ (malPrint . malEval . malRead) input

main :: IO ()
main = repl malRep ()
