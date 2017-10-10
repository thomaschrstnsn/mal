module Readline
  ( repl
  , InputT
  ) where

import System.Console.Haskeline

ini :: IO ()
ini = putStrLn "Welcome!"

repl :: (String -> InputT IO ()) -> IO ()
repl rep = do
  ini
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "user> "
      case minput of
        Nothing -> return ()
        Just input -> do
          rep input
          loop
