{-# LANGUAGE NamedFieldPuns #-}

module Printer
  ( prStr
  , prErr
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Types

keyToAst :: Key -> Ast
keyToAst k =
  case k of
    KKw kw -> AKw kw
    KInt i -> AInt i
    KBool b -> ABool b
    KStr s -> AStr s

prKey :: Key -> String
prKey = prStr . keyToAst

prStr :: Ast -> String
prStr ast =
  case ast of
    ASym s -> s
    AInt i -> show i
    AList xs -> "(" ++ unwords (map prStr xs) ++ ")"
    AVector xs -> "[" ++ unwords (map prStr xs) ++ "]"
    AMap m ->
      "{" ++
      unwords (map (\(k, v) -> unwords [prKey k, prStr v]) (Map.toList m)) ++
      "}"
    AKw s -> ':' : s
    ANil -> "nil"
    ABool True -> "true"
    ABool False -> "false"
    AStr s -> show s
    AVoid -> ""
    AFun _ -> "#function"

prErr :: Error -> String
prErr err =
  "Error: " ++
  case err of
    AggregateError errs ->
      "Multiple errors:\n" ++
      List.intercalate "\n" (map (("\t" ++) . prErr) errs)
    UnexpectedType ast t ->
      "Unexpected type: found '" ++ prStr ast ++ "'  but expected: " ++ t
    SymbolNotFound sym -> "Could not find symbol: '" ++ sym ++ "'"
    UnexpectedElementAtHead ast ->
      "Expected function or special form at head of list, but found: '" ++
      prStr ast ++ "'"
    ExpectedSymbolButFound ast ->
      "Expected symbol but found: '" ++ prStr ast ++ "'"
    UnexpectedNumberOfElementInForm {expected, actual} ->
      "Unexpected number of elements in form, expected: " ++
      show expected ++ ", but found: '" ++ prStr actual ++ "'"
    DivisionByZero -> "Division by zero attempt"
