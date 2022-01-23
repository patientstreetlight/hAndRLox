module Token where


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
    | Identifier String
    | String String
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
