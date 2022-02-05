module Eval where

import Value
import Expr


eval :: Expr -> Value
eval e = case e of
    Literal v -> v
    Unary op e' -> unary op $ eval e'
    Binary op l r -> binary op (eval l) (eval r)
    Grouping e' -> eval e'


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