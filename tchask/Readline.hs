module Readline
  ( Repl
  , repl
  ) where

import Control.Monad.IO.Class
import System.Console.Repline

type Repl a = HaskelineT IO a

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

completer :: Monad m => WordCompleter m
completer _ = return []

repl :: (String -> Repl ()) -> IO ()
repl rep = evalRepl "user> " rep [] (Word completer) ini
