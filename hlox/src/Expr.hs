module Expr where

import Data.Text

data Expr
    = Literal Literal
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Grouping Expr
    deriving Show


data Literal
    = Number Double
    | String Text
    | Bool Bool
    | Nil
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
