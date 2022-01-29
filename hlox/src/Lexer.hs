module Lexer (lexLox) where

import Data.Text (Text)
import qualified Data.Text as T
import Token
import Control.Monad.State
import Data.Char (isDigit)

data LexError
    = UnexpectedEof
    | UnterminatedString
    | UnexpectedCharacter Char

-- Lexer =~ Text -> Either LexError (a, Text)
type Lexer a = StateT Text (Either LexError) a

runLexer :: Lexer a -> Text -> Either LexError a
runLexer lexer text = fst <$> runStateT lexer text

advance :: Lexer Char
advance = do
    t <- get
    case T.uncons t of
        Nothing -> lift $ Left UnexpectedEof
        Just (c, t') -> do
            put t'
            return c

match :: Char -> Lexer Bool
match expected = do
    t <- get
    case T.uncons t of
        Just (c, t') | c == expected -> do
            put t'
            return True
        _ -> return False

tryCombineEqual :: a -> a -> Lexer a
tryCombineEqual withEqual withoutEqual = do
    hasEqual <- match '='
    return $ if hasEqual then withEqual else withoutEqual

peak :: Lexer (Maybe Char)
peak = gets (fmap fst . T.uncons)

eatToEol :: Lexer ()
eatToEol = do
    next <- peak
    case next of
        Just c | c /= '\n' -> do
            advance
            eatToEol
        _ -> return ()

-- XXX need to advance!
scanString :: Lexer String
scanString = do
    next <- peak
    case next of
        Nothing -> lift $ Left UnterminatedString
        Just '"' -> return ""
        Just c -> (c:) <$> scanString

number :: Lexer Double
number = undefined

scan2 :: Lexer [Token]
scan2 = do
    maybeC <- peak
    case maybeC of
        Nothing -> return []
        Just c -> case c of
            '(' -> advance >> 

scanToken :: Lexer Token
scanToken = do
    c <- advance
    case c of
        '(' -> return LeftParen
        ')' -> return RightParen
        '{' -> return LeftBrace
        '}' -> return RightBrace
        ',' -> return Comma
        '.' -> return Dot
        '-' -> return Minus
        '+' -> return Plus
        ';' -> return Semicolon
        '*' -> return Star
        '!' -> tryCombineEqual BangEqual Bang
        '=' -> tryCombineEqual EqualEqual Equal
        '<' -> tryCombineEqual LessEqual Less
        '>' -> tryCombineEqual GreaterEqual Greater
        '/' -> do
            isComment <- match '/'
            if isComment then do
                eatToEol
                -- XXX What to return here
                return undefined
            else
                return Slash
        '"' -> String <$> scanString
        _ | isDigit c -> Number <$> number




lexLox :: Text -> [Token]
lexLox text = undefined