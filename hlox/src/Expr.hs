module Expr where

data Expr
    = Literal Literal
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Grouping Expr


data Literal
    = Number Double
    | String String
    | Bool Bool
    | Nil


data UnaryOp
    = Negate
    | Not


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
