module Types
  ( Ast(..)
  , Key(..)
  , Function
  , Environment
  , Error(..)
  , Err
  , EvalAst
  ) where

import qualified Data.Map.Strict as Map

data Key
  = KInt Integer
  | KKw String
  | KBool Bool
  | KStr String
  deriving (Show, Ord, Eq)

data Ast
  = AList [Ast]
  | AVector [Ast]
  | AMap (Map.Map Key Ast)
  | ASym String
  | AInt Integer
  | AKw String
  | ANil
  | ABool Bool
  | AStr String
  | AFun Function
  | AComment

type Function = [Ast] -> Either Error Ast

type Environment = Map.Map String Function

data Error
  = UnexpectedType Ast
                   String
  | SymbolNotFound String
  | ExpectedFunctionAtHead Ast
  | ApplyWhenNoList Ast
  | DivisionByZero
  | AggregateError [Error]

type Err a = Either Error a

type EvalAst = Err Ast
