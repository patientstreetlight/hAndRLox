module Token where

import Data.Text (Text)

data Token
    -- Single-character tokens
    = LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star

    -- One or two character tokens
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

    -- Literals
    | Identifier Text
    | String Text
    | Number Double

    -- Keywords
    | And
    | Class
    | Else
    | TokFalse
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | TokTrue
    | Var
    | While

    | Eof
    deriving (Show, Eq)


-- A token combined with some debug information to better report errors
-- back to the user.
data DebugToken = DebugToken
    { dtToken :: Token
    , dtLexeme :: Text
    , dtLine :: Int
    } deriving Show