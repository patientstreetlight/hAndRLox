module AST where

import Data.Text
import Value


data Stmt
    = Expr Expr
    | Print Expr
    | Decl Text (Maybe Expr)
    | Block [Stmt]
    deriving Show


data Expr
    = Literal Value
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Grouping Expr
    | Identifier Text
    | Assign Text Expr
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
    deriving Show
