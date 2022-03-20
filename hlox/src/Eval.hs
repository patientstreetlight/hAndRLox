{-# LANGUAGE GeneralisedNewtypeDeriving, LambdaCase, OverloadedStrings #-}

module Eval where

import AST
import Env (Env, newNestedEnv, newRootEnv)
import qualified Env as E
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Except
import Data.Map (Map)

data Value
    = Number Double
    | Bool Bool
    | String Text
    | Nil
    | Fun Function
    | Obj Object
    deriving Show

instance Eq Value where
    Number a == Number b = a == b
    Bool a   == Bool b   = a == b
    String a == String b = a == b
    Nil      == Nil      = True
    Obj    a == Obj b    = a == b
    a        == b        = error $ "Incomparable values " ++ show a ++ " " ++ show b

newtype Object = Object
    { fields :: IORef (Map Text Value)
    } deriving Eq

instance Show Object where
    show o = "Object"

type Cell = IORef Value

type LoxEnv = Env Text Cell

data Function = Function
    { params :: [Text]
    , call :: Lox ()
    , name :: Text
    , closure :: LoxEnv
    }

instance Show Function where
    show f = T.unpack $ name f


data ControlFlow
    = CfReturn Value
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
    Print e -> loxPrint e
    Expr e -> void $ eval e
    Decl var e -> declVar var e
    Block stmts -> execBlock stmts
    If condition thenBranch mElseBranch -> ifThenElse condition thenBranch mElseBranch
    While condition loopBody -> while condition loopBody
    DeclFun fnName params funBody -> declFun fnName params funBody
    Return mExpr -> loxReturn mExpr
    DeclClass className methods -> declClass className methods


loxPrint :: Expr -> Lox ()
loxPrint e = do
    val <- eval e
    liftIO $ print val

declVar :: Text -> Maybe Expr -> Lox ()
declVar var mInitExpr = do
    val <- case mInitExpr of
        Nothing -> return Nil
        Just e -> eval e
    valRef <- liftIO $ newIORef val -- XXX Does val need to be forced?
    modify $ E.define var valRef

execBlock :: [Stmt] -> Lox ()
execBlock stmts = do
    env <- get
    let nestedEnv = newNestedEnv env
    put nestedEnv
    mapM_ exec stmts
    put env

ifThenElse :: Expr -> Stmt -> Maybe Stmt -> Lox ()
ifThenElse condition thenBranch mElseBranch = do
    c <- eval condition
    if truthy c then
        exec thenBranch
    else
        forM_ mElseBranch exec

while :: Expr -> Stmt -> Lox ()
while condition loopBody = loop
  where
    loop = do
        c <- eval condition
        when (truthy c) $ do
            exec loopBody
            loop

loxReturn mExpr = do
    retVal <-
        case mExpr of
            Nothing -> return Nil
            Just e -> eval e
    throwError $ CfReturn retVal

declClass :: Text -> [Method] -> Lox ()
declClass className methods = do
    -- XXX handle custom constructor
    env <- get
    let createInstance = do
            fs <- liftIO $ newIORef M.empty
            let this = Obj $ Object fs
            thisRef <- liftIO $ newIORef this
            let thisBoundEnv = E.define "this" thisRef $ newNestedEnv env
            forM_ methods $ \(Method methodName params methodBody) -> do
                let boundMethod = Function
                                 { params = params
                                 , name = methodName
                                 , closure = thisBoundEnv
                                 , call = mapM_ exec methodBody
                                 }
                liftIO $ modifyIORef fs $ M.insert methodName $ Fun boundMethod
            methodsMap <- liftIO $ readIORef fs
            case M.lookup "init" methodsMap of
                -- XXX Need to somehow call init().  Tricky for a couple reasons:
                -- - need to make sure its closure is set up properly so "this" is in scope
                -- - need to make sure any arguments passed to the constructor are
                --   visible when it runs.
                Just (Fun init) -> return ()
                _ -> return ()
            throwError $ CfReturn this

    let ctor = Function
              { params = []
              , name = className
              , closure = env
              , call = createInstance
              }
    ctorRef <- liftIO $ newIORef $ Fun ctor
    modify $ E.define className ctorRef

declFun :: Text -> [Text] -> [Stmt] -> Lox ()
declFun fnName params funBody = do
    env <- get
    -- XXX function should be able to call itself recursively, but currently can't
    let fn = Function
            { params = params
            , name = fnName
            , closure = env
            , call = mapM_ exec funBody
            }
    funRef <- liftIO $ newIORef $ Fun fn
    modify $ E.define fnName funRef

truthy :: Value -> Bool
truthy = \case
    Nil -> False
    Bool False -> False
    _ -> True

eval :: Expr -> Lox Value
eval e = case e of
    Literal (LitBool b) -> return $ Bool b
    Literal (LitNumber a) -> return $ Number a
    Literal (LitString s) -> return $ String s
    Literal LitNil -> return Nil
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
    Get objExpr field -> getExpr objExpr field
    Set objExpr field valExpr -> setExpr objExpr field valExpr

setExpr :: Expr -> Text -> Expr -> Lox Value
setExpr objExpr fieldName valExpr = do
    lhs <- eval objExpr
    case lhs of
        Obj obj -> do
            val <- eval valExpr
            liftIO $ modifyIORef' (fields obj) $ M.insert fieldName val
            return val
        _ -> error "Only instances have fields"

getExpr :: Expr -> Text -> Lox Value
getExpr e fieldName = do
    val <- eval e
    case val of
        Obj obj -> do
            fs <- liftIO $ readIORef $ fields obj
            case M.lookup fieldName fs of
                Nothing -> error $ "Undefined property " ++ T.unpack fieldName
                Just property -> return property
        _ -> error "Only instances have properties"

runFunction :: Expr -> [Expr] -> Lox Value
runFunction calleeExpr argExprs = do
    callee <- eval calleeExpr
    case callee of
        Fun f | length (params f) /= length argExprs ->
            error $ "wrong number of arguments to function " ++ show f
        Fun f -> do
            args <- mapM eval argExprs
            argCells <- liftIO $ mapM newIORef args
            let argBindings = zip (params f) argCells
                fEnv = E.defineAll argBindings $ E.newNestedEnv $ closure f
            oldEnv <- get
            put fEnv
            retVal <- (call f >> return Nil) `catchError` \e ->
                case e of
                    CfReturn v -> return v
                    _ -> error "unhandled control flow"
            put oldEnv
            return retVal
        v -> error $ show v ++ " is not a function"

resolve :: Text -> Lox Cell
resolve var = do
    mVal <- gets $ E.resolve var
    case mVal of
        Just val -> return val
        Nothing -> error $ "undefined variable: " ++ T.unpack var

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
    And -> if truthy a then b else a
    Or -> if truthy a then a else b


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
