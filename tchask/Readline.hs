module Readline
  ( repl
  , InputT
  , Repl
  ) where

import Control.Monad.State.Strict
import System.Console.Haskeline

ini :: IO ()
ini = putStrLn "Welcome!"

type Repl a = (String -> InputT (StateT a IO) ())

repl :: Repl a -> a -> IO ()
repl rep s = do
  ini
  evalStateT (runInputT defaultSettings loop) s
  where
    loop = do
      minput <- getInputLine "user> "
      case minput of
        Nothing -> return ()
        Just input -> do
          rep input
          loop
