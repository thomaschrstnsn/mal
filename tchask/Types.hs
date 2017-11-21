{-# LANGUAGE FlexibleContexts #-}

module Types
  ( Ast(..)
  , Key(..)
  , Function(..)
  , NativeFunction
  , Error(..)
  , Err
  , EvalAst
  , Environment(..)
  , EvalM(..)
  , ErrM
  ) where

import Control.Monad.Except
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
  | ANativeFun NativeFunction
  | AFun Function
  | AVoid

type NativeFunction = [Ast] -> EvalAst

data Function =
  Function Environment
           NativeFunction

data Error
  = UnexpectedType Ast
                   String
  | SymbolNotFound String
  | UnexpectedElementAtHead Ast
  | DivisionByZero
  | ExpectedSymbolButFound Ast
  | UnexpectedNumberOfElementInForm { unoeifExpected :: Integer
                                    , unoeifActual :: Ast
                                    , unoeifForm :: String }
  | UnevenNumberOfElementsInLetBinding
  | ArgumentCountMismatch { acmExpected :: Integer
                          , acmActualCount :: Integer
                          , acmArguments :: [Ast] }
  | AggregateError [Error]

type Err a = Either Error a

type EvalAst = Err Ast

data Environment = Env
  { outer :: Maybe Environment
  , envData :: Map.Map String Ast
  }

class (Monad m, MonadError Error m) =>
      ErrM m

class (Monad m, ErrM m) =>
      EvalM m where
  getEnv :: String -> m Ast
  setEnv :: String -> Ast -> m ()
  newEnv :: m Environment
  withEnv :: Environment -> m a -> m (Environment, a)
