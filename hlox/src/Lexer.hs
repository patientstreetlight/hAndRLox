{-# LANGUAGE OverloadedStrings #-}

module Lexer (lexLox) where

import Data.Text (Text)
import qualified Data.Text as T
import Token
import Control.Applicative
import Data.List (foldl1')
import Control.Monad.State
import Data.Char (isDigit, isAlpha, isAlphaNum)

data LexError
    = UnexpectedEof
    | UnterminatedString
    | UnexpectedCharacter Char


data LexState = LexState
    { input :: Text
    , currentTokenSize :: Int
    , currentLine :: Int
    , tokensReversed :: [DebugToken]
    , errors :: [LexError]
    }

initState :: Text -> LexState
initState input = LexState input 0 1 [] []

type Lexer a = State LexState a

lexLox :: Text -> Either [LexError] [DebugToken]
lexLox input = evalState scanTokens (initState input)

isAtEnd :: Lexer Bool
isAtEnd = do
    s <- get
    let current = currentTokenSize s
        source = input s
    return $ case T.compareLength source current of
        GT -> False
        _  -> True

scanTokens :: Lexer (Either [LexError] [DebugToken])
scanTokens = do
    let loop = do
            done <- isAtEnd
            unless done $ do
                scanToken
                loop
    loop
    s <- get
    let errs = errors s
        toks = reverse $ tokensReversed s
    return $ if null errs then Right toks else Left errs

advance :: Lexer Char
advance = do
    s <- get
    let tokenSize = currentTokenSize s
    put $ s { currentTokenSize = tokenSize + 1 }
    return $ T.index (input s) tokenSize


addToken :: Token -> Lexer ()
addToken token = addTokenWith $ const token

addTokenWith :: (Text -> Token) -> Lexer ()
addTokenWith mkToken = do
    s <- get
    let (lexeme, input') = T.splitAt (currentTokenSize s) (input s)
        tokens = tokensReversed s
        debugToken = DebugToken (mkToken lexeme) lexeme $ currentLine s
    put $ s { input = input', currentTokenSize = 0, tokensReversed = debugToken : tokens }

skip :: Lexer ()
skip = do
    s <- get
    let input' = T.drop (currentTokenSize s) (input s)
    put $ s { input = input', currentTokenSize = 0 }

match :: Char -> Lexer Bool
match expected = do
    done <- isAtEnd
    if done then
        return False
    else do
        s <- get
        if T.index (input s) (currentTokenSize s) /= expected then
            return False
        else do
            put $ s { currentTokenSize = currentTokenSize s + 1 }
            return True

peek :: Lexer (Maybe Char)
peek = do
    atEnd <- isAtEnd
    if atEnd then
        return Nothing
    else do
        s <- get
        return $ Just $ T.index (input s) (currentTokenSize s)

-- XXX What about handling nested strings with \", or newline characters like \n
string :: Lexer ()
string = do
    let loop = do
            p <- peek
            case p of
                Nothing -> return () -- XXX Should be unterminated string error
                Just '"' -> void advance
                Just c -> do
                    when (c == '\n') $ do
                        s <- get
                        put $ s { currentLine = currentLine s + 1 }
                    advance
                    loop
    loop
    addTokenWith (String . T.init . T.tail)

number :: Lexer ()
number = do
    let eatDigits = do
            p <- peek
            case p of
                Just c | isDigit c -> do
                    advance
                    eatDigits
                _ -> return ()
    eatDigits
    -- The next character is not a digit, but it might be a '.' before a fractional part.
    p <- peek2
    case p of
        Just ('.', d) | isDigit d -> do
            advance
            eatDigits
        _ -> return ()
    addTokenWith (Number . read . T.unpack) -- XXX Can this avoid the String conversion?

peek2 :: Lexer (Maybe (Char, Char))
peek2 = do
    s <- get
    let source = input s
        current = currentTokenSize s
    return $ case T.compareLength source (current + 2) of
        LT -> Nothing
        _ -> Just (T.index source current, T.index source (current + 1))

scanToken :: Lexer ()
scanToken = do
    c <- advance
    case c of
        '(' -> addToken LeftParen
        ')' -> addToken RightParen
        '{' -> addToken LeftBrace
        '}' -> addToken RightBrace
        ',' -> addToken Comma
        '.' -> addToken Dot
        '-' -> addToken Minus
        '+' -> addToken Plus
        ';' -> addToken Semicolon
        '*' -> addToken Star
        '!' -> do
            matchEqual <- match '='
            addToken $ if matchEqual then BangEqual else Bang
        '=' -> do
            matchEqual <- match '='
            addToken $ if matchEqual then EqualEqual else Equal
        '<' -> do
            matchEqual <- match '='
            addToken $ if matchEqual then LessEqual else Less
        '>' -> do
            matchEqual <- match '='
            addToken $ if matchEqual then GreaterEqual else Greater 
        '/' -> do
            matchSlash <- match '/'
            if matchSlash then do
                let loop = do
                        p <- peek
                        when (p /= Just '\n') $ do
                            advance
                            loop
                loop
            else
                addToken Slash
        ' ' -> skip
        '\r' -> skip
        '\t' -> skip
        '\n' -> do
            s <- get
            put $ s { currentLine = currentLine s + 1 }
            skip
        '"' -> string
        _ | isDigit c -> number
        _ | isAlpha c -> identifier
        _ -> skip -- XXX Should error that this character is unexpected


identifier :: Lexer ()
identifier = do
    let loop = do
            p <- peek
            case p of
                Just c | isAlphaNum c -> do
                    advance
                    loop
                _ -> return ()
    loop
    addTokenWith toIdentifierOrKeyword


toIdentifierOrKeyword :: Text -> Token
toIdentifierOrKeyword lexeme = case lexeme of
    "and" -> And
    "class" -> Class
    "else" -> Else
    "false" -> TokFalse
    "fun" -> Fun
    "for" -> For
    "if" -> If
    "nil" -> Nil
    "or" -> Or
    "print" -> Print
    "return" -> Return
    "super" -> Super
    "this" -> This
    "true" -> TokTrue
    "var" -> Var
    "while" -> While
    _ -> Identifier lexeme