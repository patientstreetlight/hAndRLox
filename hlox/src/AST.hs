module AST where

import Data.Text


data Stmt
    = Expr Expr
    | Print Expr
    | Decl Text (Maybe Expr)
    | Block [Stmt]
    | If Expr Stmt (Maybe Stmt)
    | While Expr Stmt
    | DeclFun Text [Text] [Stmt]
    | Return (Maybe Expr)
    | DeclClass Text [Method]
    deriving Show

data Method = Method Text [Text] [Stmt]
    deriving Show

data Expr
    = Literal Literal
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Grouping Expr
    | Identifier Text
    | Assign Text Expr
    | Call Expr [Expr]
    | Get Expr Text
    | Set Expr Text Expr
    deriving Show

data Literal
    = LitNumber Double
    | LitBool Bool
    | LitString Text
    | LitNil
    deriving Show

data UnaryOp
    = Negate
    | Not
    deriving Show


data BinaryOp
    = Equals
    | NotEquals
    | LessThan
    | LessThanEquals
    | GreaterThan
    | GreaterThanEquals
    | Add
    | Sub
    | Mult
    | Divide
    | And
    | Or
    deriving Show
