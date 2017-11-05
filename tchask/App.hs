{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module App
  ( run
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Environment as Env
import Types

newtype AppM a = AppM
  { runAppM :: (ExceptT Error (State Environment)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError Error
             , MonadState Environment
             )

instance EvalM AppM where
  getEnv s = do
    env <- get
    Env.findEnv s env
  setEnv s a = do
    env <- get
    put $ Env.setEnv s a env
  newEnv = do
    env <- get
    return $ Env.newEnv env
  withEnv env expr = do
    orig <- get
    put env
    res <- expr
    next <- get
    put orig
    return (next, res)

instance (MonadError Error AppM) => ErrM AppM

run :: AppM a -> Environment -> (Either Error a, Environment)
run a = runState $ runExceptT (runAppM a)
