import System.IO (hFlush, stdout)
import Control.Monad (mapM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Data.Traversable as DT

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)
import Env (Env, env_new, env_bind, env_get, env_set)
import Core as Core

-- read
mal_read :: String -> IOThrows MalVal
mal_read str = read_str str

-- eval
eval_ast :: MalVal -> Env -> IOThrows MalVal
eval_ast sym@(MalSymbol _) env = env_get env sym
eval_ast ast@(MalList lst m) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalList new_lst m
eval_ast ast@(MalVector lst m) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalVector new_lst m
eval_ast ast@(MalHashMap lst m) env = do
    new_hm <- DT.mapM (\x -> (eval x env)) lst
    return $ MalHashMap new_hm m
eval_ast ast env = return ast

let_bind :: Env -> [MalVal] -> IOThrows Env
let_bind env [] = return env
let_bind env (b:e:xs) = do
    evaled <- eval e env
    x <- liftIO $ env_set env b evaled
    let_bind env xs

apply_ast :: MalVal -> Env -> IOThrows MalVal
apply_ast ast@(MalList [] _) env = do
    return ast
apply_ast ast@(MalList (MalSymbol "def!" : args) _) env = do
    case args of
         (a1@(MalSymbol _): a2 : []) -> do
            evaled <- eval a2 env
            liftIO $ env_set env a1 evaled
         _ -> throwStr "invalid def!"
apply_ast ast@(MalList (MalSymbol "let*" : args) _) env = do
    case args of
         (a1 : a2 : []) -> do
            params <- (_to_list a1)
            let_env <- liftIO $ env_new $ Just env
            let_bind let_env params
            eval a2 let_env
         _ -> throwStr "invalid let*"
apply_ast ast@(MalList (MalSymbol "do" : args) _) env = do
    case args of
         ([]) -> return Nil
         _  -> do
            el <- eval_ast (MalList args Nil) env
            case el of
                 (MalList lst _) -> return $ last lst

apply_ast ast@(MalList (MalSymbol "if" : args) _) env = do
    case args of
         (a1 : a2 : a3 : []) -> do
            cond <- eval a1 env
            if cond == MalFalse || cond == Nil
                then eval a3 env
                else eval a2 env
         (a1 : a2 : []) -> do
            cond <- eval a1 env
            if cond == MalFalse || cond == Nil
                then return Nil
                else eval a2 env
         _ -> throwStr "invalid if"
apply_ast ast@(MalList (MalSymbol "fn*" : args) _) env = do
    case args of
         (a1 : a2 : []) -> do
            params <- (_to_list a1)
            return $ (_func
                      (\args -> do
                        fn_env1 <- liftIO $ env_new $ Just env
                        fn_env2 <- liftIO $ env_bind fn_env1 params args
                        eval a2 fn_env2))
         _ -> throwStr "invalid fn*"
apply_ast ast@(MalList _ _) env = do
    el <- eval_ast ast env
    case el of
         (MalList ((Func (Fn f) _) : rest) _) ->
            f $ rest
         el ->
            throwStr $ "invalid apply: " ++ (show el)

eval :: MalVal -> Env -> IOThrows MalVal
eval ast env = do
    case ast of
         (MalList _ _) -> apply_ast ast env
         _             -> eval_ast ast env


-- print
mal_print :: MalVal -> String
mal_print exp = show exp

-- repl

rep :: Env -> String -> IOThrows String
rep env line = do
    ast <- mal_read line
    exp <- eval ast env
    return $ mal_print exp

repl_loop :: Env -> IO ()
repl_loop env = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop env
        Just str -> do
            res <- runExceptT $ rep env str
            out <- case res of
                Left (StringError str) -> return $ "Error: " ++ str
                Left (MalValError mv) -> return $ "Error: " ++ (show mv)
                Right val -> return val
            putStrLn out
            hFlush stdout
            repl_loop env

main = do
    load_history

    repl_env <- env_new Nothing

    -- core.hs: defined using Haskell
    (mapM (\(k,v) -> (env_set repl_env (MalSymbol k) v)) Core.ns)

    -- core.mal: defined using the language itself
    runExceptT $ rep repl_env "(def! not (fn* (a) (if a false true)))"

    repl_loop repl_env
