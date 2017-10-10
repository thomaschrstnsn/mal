module Printer
  ( prStr
  ) where

import qualified Data.Map.Strict as Map
import Types

prStr :: Ast -> String
prStr ast =
  case ast of
    ASym s -> s
    AInt i -> show i
    AList xs -> "(" ++ unwords (map prStr xs) ++ ")"
    AVector xs -> "[" ++ unwords (map prStr xs) ++ "]"
    AMap m ->
      "{" ++
      unwords (map (\(k, v) -> unwords [prStr k, prStr v]) (Map.toList m)) ++
      "}"
    AKw s -> ':' : s
    ANil -> "nil"
    ABool True -> "true"
    ABool False -> "false"
    AStr s -> show s
    AComment -> ""
