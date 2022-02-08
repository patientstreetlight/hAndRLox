{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Eval where

import Value
import AST
import Env (Env, newNestedEnv, newRootEnv)
import qualified Env as E
import Control.Monad.State
import Data.Maybe (fromMaybe)


newtype Lox a = Lox
    { runLox :: StateT Env IO a
    } deriving (Functor, Applicative, Monad, MonadState Env, MonadIO)

execLox :: Lox a -> IO ()
execLox lox = void $ execStateT (runLox lox) newRootEnv

runProgram :: [Stmt] -> IO ()
runProgram stmts = execLox (mapM_ exec stmts)

exec :: Stmt -> Lox ()
exec s = case s of
    Print e -> do
        val <- eval e
        liftIO $ print val
    Expr e -> void $ eval e
    Decl var mInitExpr -> do
        val <- case mInitExpr of
            Nothing -> return Nil
            Just e -> eval e
        modify $ E.define var val
    Block stmts -> do
        env <- get
        let nestedEnv = newNestedEnv env
        put nestedEnv
        mapM_ exec stmts
        mEnv' <- gets E.parent
        maybe (error "nested env somehow had no parent") put mEnv'

eval :: Expr -> Lox Value
eval e = case e of
    Literal v -> return v
    Unary op e' -> unary op <$> eval e'
    Binary op l r -> binary op <$> eval l <*> eval r
    Grouping e' -> eval e'
    Identifier var -> gets $ E.get var
    Assign var e' -> do
        val <- eval e'
        modify $ E.set var val
        return val


unary :: UnaryOp -> Value -> Value
unary op = case op of
    Negate -> negateVal
    Not -> notVal


negateVal :: Value -> Value
negateVal v = case v of
    Number d -> Number $ -d
    _ -> error "can only negate numbers"


notVal :: Value -> Value
notVal v = Bool $ case v of
    Bool b -> not b
    Nil -> True
    _ -> False


binary :: BinaryOp -> Value -> Value -> Value
binary op a b = case op of
    Equals -> Bool $ a == b
    NotEquals -> Bool $ a /= b
    LessThan -> cmp (<) a b
    LessThanEquals -> cmp (<=) a b
    GreaterThan -> cmp (>) a b
    GreaterThanEquals -> cmp (>=) a b
    Add -> add a b
    Sub -> arithmetic (-) a b
    Mult -> arithmetic (*) a b
    Divide -> arithmetic (/) a b


cmp :: (Double -> Double -> Bool) -> Value -> Value -> Value
cmp op a b = case (a, b) of
    (Number n1, Number n2) -> Bool $ n1 `op` n2
    _ -> error "can only compare numbers"


add :: Value -> Value -> Value
add a b = case (a, b) of
    (Number n1, Number n2) -> Number $ n1 + n2
    (String s1, String s2) -> String $ s1 <> s2
    _ -> error "can only add numbers or strings"


arithmetic :: (Double -> Double -> Double) -> Value -> Value -> Value
arithmetic op a b = case (a, b) of
    (Number n1, Number n2) -> Number $ n1 `op` n2
    _ -> error "can only do arithmetic on numbers"