{-# LANGUAGE GeneralisedNewtypeDeriving, LambdaCase #-}

module Eval where

import Value
import AST
import Env (Env, newNestedEnv, newRootEnv)
import qualified Env as E
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except

data ValOrFun
    = Val Value
    | Fun Function

instance Show ValOrFun where
    show (Val v) = show v
    show (Fun f) = show f

type Cell = IORef ValOrFun

type LoxEnv = Env Text Cell

data ControlFlow
    = CfReturn ValOrFun
    | CfBreak (Maybe Text)
    | CfContinue

newtype Lox a = Lox
    { runLox :: ExceptT ControlFlow (StateT LoxEnv IO) a
    } deriving (Functor, Applicative, Monad, MonadState LoxEnv, MonadIO, MonadError ControlFlow)

execLox :: Lox a -> IO ()
execLox lox = void $ execStateT (runExceptT $ runLox lox) newRootEnv

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
            Nothing -> return $ Val Nil
            Just e -> eval e
        valRef <- liftIO $ newIORef val -- XXX Does val need to be forced?
        modify $ E.define var valRef
    Block stmts -> do
        env <- get
        let nestedEnv = newNestedEnv env
        put nestedEnv
        mapM_ exec stmts
        put env
    If condition thenBranch mElseBranch -> do
        c <- eval condition
        if truthy c then
            exec thenBranch
        else
            forM_ mElseBranch exec
    While condition loopBody -> do
        let loop = do
                c <- eval condition
                when (truthy c) $ do
                    exec loopBody
                    loop
        loop
    DeclFun fnName params funBody -> do
        env <- get
        let fn =
              Function
                { params = params
                , name = fnName
                , closure = env
                , call = mapM_ exec funBody
                }
        funRef <- liftIO $ newIORef $ Fun fn
        modify $ E.define fnName funRef
    Return mExpr -> do
        retVal <-
            case mExpr of
                Nothing -> return $ Val Nil
                Just e -> eval e
        throwError $ CfReturn retVal


data Function = Function
    { params :: [Text]
    , call :: Lox ()
    , name :: Text
    , closure :: LoxEnv
    }

instance Show Function where
    show f = T.unpack $ name f

truthy :: ValOrFun -> Bool
truthy = \case
    Val Nil -> False
    Val (Bool False) -> False
    _ -> True

eval :: Expr -> Lox ValOrFun
eval e = case e of
    Literal v -> return $ Val v
    Unary op e' -> unary op <$> eval e'
    Binary op l r -> binary op <$> eval l <*> eval r
    Grouping e' -> eval e'
    Identifier var -> do
        cell <- resolve var
        liftIO $ readIORef cell
    Assign var e' -> do
        cell <- resolve var
        val <- eval e'
        liftIO $ writeIORef cell val
        return val
    Call calleeExpr argExprs ->
        runFunction calleeExpr argExprs

runFunction :: Expr -> [Expr] -> Lox ValOrFun
runFunction calleeExpr argExprs = do
    callee <- eval calleeExpr
    case callee of
        Val v -> error $ show v ++ " is not a function"
        Fun f | length (params f) /= length argExprs ->
            error $ "wrong number of arguments to function " ++ show f
        Fun f -> do
            args <- mapM eval argExprs
            argCells <- liftIO $ mapM newIORef args
            let argBindings = zip (params f) argCells
                fEnv = E.defineAll argBindings $ E.newNestedEnv $ closure f
            oldEnv <- get
            put fEnv
            retVal <- (call f >> return (Val Nil)) `catchError` \e ->
                case e of
                    CfReturn v -> return v
                    _ -> error "unhandled control flow"
            put oldEnv
            return retVal

resolve :: Text -> Lox Cell
resolve var = do
    mVal <- gets $ E.resolve var
    case mVal of
        Just val -> return val
        Nothing -> error $ "undefined variable: " ++ T.unpack var

unary :: UnaryOp -> ValOrFun -> ValOrFun
unary op = case op of
    Negate -> negateVal
    Not -> notVal


negateVal :: ValOrFun -> ValOrFun
negateVal v = case v of
    Val (Number d) -> Val $ Number $ -d
    _ -> error "can only negate numbers"


notVal :: ValOrFun -> ValOrFun
notVal v = Val . Bool $ case v of
    Val (Bool b) -> not b
    Val Nil -> True
    _ -> False


binary :: BinaryOp -> ValOrFun -> ValOrFun -> ValOrFun
binary op a b = case op of
    Equals -> valsOnly (\a b -> Bool $ a == b)
    NotEquals -> valsOnly (\a b -> Bool $ a /= b)
    LessThan -> valsOnly (cmp (<))
    LessThanEquals -> valsOnly (cmp (<=))
    GreaterThan -> valsOnly (cmp (>))
    GreaterThanEquals -> valsOnly (cmp (>=))
    Add -> valsOnly add
    Sub -> valsOnly (arithmetic (-))
    Mult -> valsOnly (arithmetic (*))
    Divide -> valsOnly (arithmetic (/))
    And -> if truthy a then b else a
    Or -> if truthy a then a else b
  where
    valsOnly :: (Value -> Value -> Value) -> ValOrFun
    valsOnly f = case (a, b) of
        (Val a', Val b') -> Val $ f a' b'
        _ -> error "Can't perform operation on functions"


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