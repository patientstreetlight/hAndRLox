module AST where

import Data.Text
import Value

data Stmt
    = Expr Expr
    | Print Expr
    deriving Show


data Expr
    = Literal Value
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Grouping Expr
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
