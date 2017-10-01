module Types
  ( Ast(..)
  ) where

data Ast
  = AList [Ast]
  | ASym String
  | AInt Integer
  deriving (Show, Eq)
