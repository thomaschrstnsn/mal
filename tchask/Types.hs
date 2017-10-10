module Types
  ( Ast(..)
  ) where

import qualified Data.Map.Strict as Map

data Ast
  = AList [Ast]
  | AVector [Ast]
  | AMap (Map.Map Ast Ast)
  | ASym String
  | AInt Integer
  | AKw String
  | ANil
  | ABool Bool
  | AStr String
  | AComment
  deriving (Show, Eq, Ord)
