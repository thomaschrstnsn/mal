module Printer
  ( prStr
  ) where

import Types

prStr :: Ast -> String
prStr ast =
  case ast of
    ASym s -> s
    AInt i -> show i
    AList xs -> "(" ++ unwords (map prStr xs) ++ ")"
