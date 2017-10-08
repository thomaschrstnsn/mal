module Types
  ( Ast(..)
  ) where

data Ast
  = AList [Ast]
  | ASym String
  | AInt Integer
  | AKw String
  | ANil
  | ABool Bool
  | AStr String
  | AComment
  deriving (Show, Eq)
