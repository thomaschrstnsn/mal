module Environment
  ( setEnv
  , findEnv
  , newEnv
  ) where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Types (Ast, Environment(..), ErrM, Error(..))

setEnv :: String -> Ast -> Environment -> Environment
setEnv key val env = env {envData = Map.insert key val $ envData env}

findEnv :: ErrM m => String -> Environment -> m Ast
findEnv key env =
  case Map.lookup key (envData env) of
    Just ast -> return ast
    Nothing ->
      case outer env of
        Just outerEnv -> findEnv key outerEnv
        Nothing -> throwError $ SymbolNotFound key

newEnv :: Environment -> Environment
newEnv env = Env {envData = Map.empty, outer = Just env}
