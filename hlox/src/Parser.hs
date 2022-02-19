{-# LANGUAGE LambdaCase #-}

module Parser where
import Token
import qualified Token as T
import Control.Applicative
import Data.List (foldl')
import AST
import qualified AST as E
import Data.Foldable (asum)
import Value
import qualified Value as V
import Data.Text (Text)
import Data.Maybe (fromMaybe)


newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])] }

-- XXX better errors
parseLox :: [Token] -> [Stmt]
parseLox tokens =
    case runParser loxFile tokens of
        [] -> error "parse failure"
        [(parsed, [])] -> parsed
        [(parsed, _)] -> error "unconsumed input"
        xs -> error $ "ambiguous parse: " ++ show xs

loxFile :: Parser [Stmt]
loxFile = many declaration <* eof

declaration :: Parser Stmt
declaration = varDecl <|> stmt

varDecl :: Parser Stmt
varDecl = var <* semicolon

-- a variable declation _without_ a trailing semicolon
var :: Parser Stmt
var = Decl <$ token Var <*> identifier <*> optional (token T.Equal *> expression)

semicolon :: Parser Token
semicolon = token T.Semicolon

stmt :: Parser Stmt
stmt = choice
    [ E.Print <$ token T.Print <*> expression <* semicolon
    , E.Block <$ token T.LeftBrace <*> (many declaration <* token T.RightBrace)
    , E.If <$ token T.If <*>
          (token T.LeftParen *> expression <* token T.RightParen) <*>
          stmt <*>
          optional (token T.Else *> stmt)
    , E.While <$ token T.While <*>
          (token T.LeftParen *> expression <* token T.RightParen) <*>
          stmt
    , forStmt
    , E.Expr <$> expression <* semicolon
    ]

forStmt :: Parser Stmt
forStmt = mkForLoop <$ token T.For <*>
    (token T.LeftParen *> optional init <* semicolon) <*>
    (optional expression <* semicolon) <*>
    (optional expression <* token T.RightParen) <*>
    stmt
  where
    init = var <|> (E.Expr <$> expression)
    mkForLoop :: Maybe Stmt -> Maybe Expr -> Maybe Expr -> Stmt -> Stmt
    mkForLoop mInitializer mCondition mIncrement loopBody = forLoop
      where
        condition = fromMaybe (E.Literal $ Bool True) mCondition
        loopBodyWithIncrement =
            case mIncrement of
                Nothing -> loopBody
                Just increment -> E.Block
                    [ loopBody
                    , E.Expr increment
                    ]
        whileLoop = E.While condition loopBodyWithIncrement
        forLoop = case mInitializer of
            Nothing -> whileLoop
            Just initializer -> E.Block [ initializer, whileLoop ]

eof :: Parser ()
eof =
    Parser $ \case
        [] -> [((), [])]
        _ -> empty

satisfies :: (Token -> Bool) -> Parser Token
satisfies p =
    Parser $ \case
        t : ts | p t -> [ (t, ts) ]
        _ -> []

token :: Token -> Parser Token
token t = satisfies (== t)

pluck :: (Token -> Maybe a) -> Parser a
pluck f =
    Parser $ \case
        [] -> []
        t : ts -> case f t of
            Nothing -> []
            Just a -> [(a, ts)]

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \tokens ->
            [ (f a, ts)
            | (a, ts) <- p tokens
            ]


instance Applicative Parser where
    pure a =
        Parser $ \tokens -> [(a, tokens)]

    Parser pF <*> Parser pA =
        Parser $ \tokens ->
            [ (f a, ts')
            | (f, ts) <- pF tokens
            , (a, ts') <- pA ts
            ]


instance Alternative Parser where
    empty = Parser $ const []
    Parser pA <|> Parser pB =
        Parser $ \tokens -> pA tokens ++ pB tokens


sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) <|> pure []


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = foldl' applyOp <$> p <*> many ((,) <$> op <*> p)
  where
    applyOp acc (f, rhs) = f acc rhs


chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = foo <$> p <*> many ((,) <$> op <*> p)
  where
    foo :: b -> [(b -> b -> b, b)] -> b
    foo x rest =
        let (rest', x') = shift x rest
        in foldr applyOp x' rest'
    applyOp (lhs, f) acc = f lhs acc

shift :: a -> [(a -> a -> a, a)] -> ([(a, a -> a -> a)], a)
shift = go []
  where
    go acc x [] = (acc, x)
    go acc x ((f, y) : rest) = go ((x, f) : acc) y rest


choice :: [Parser a] -> Parser a
choice = asum

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = logicOr `chainr1` (assign <$ token T.Equal)

assign :: Expr -> Expr -> Expr
assign l r = case l of
    E.Identifier i -> Assign i r
    _ -> error "Invalid assignment target"

logicOr :: Parser Expr
logicOr = logicAnd `chainr1` (E.Binary E.Or <$ token T.Or)

logicAnd :: Parser Expr
logicAnd = equality `chainr1` (E.Binary E.And <$ token T.And)

equality :: Parser Expr
equality = comparison `chainl1` eqOperator
  where
    eqOperator = choice
        [ Binary Equals <$ token EqualEqual
        , Binary NotEquals <$ token BangEqual
        ]

comparison :: Parser Expr
comparison = term `chainl1` compOperator
  where
    compOperator = choice
        [ Binary LessThan <$ token Less
        , Binary LessThanEquals <$ token LessEqual
        , Binary GreaterThan <$ token Greater
        , Binary GreaterThanEquals <$ token GreaterEqual
        ]

term :: Parser Expr
term = factor `chainl1` addOp
  where
    addOp = choice
        [ Binary Add <$ token Plus
        , Binary Sub <$ token Minus
        ]

factor :: Parser Expr
factor = unary `chainl1` mulOp
  where
    mulOp = choice
        [ Binary Mult <$ token Star
        , Binary Divide <$ token Slash
        ]

unary :: Parser Expr
unary = choice
    [ Unary Negate <$ token Minus <*> unary
    , Unary Not <$ token Bang <*> unary
    , primary
    ]

primary :: Parser Expr
primary = (E.Literal <$> literal) <|> (E.Identifier <$> identifier) <|> parens

parens :: Parser Expr
parens = (Grouping <$ token LeftParen) <*> expression <* token RightParen

literal :: Parser Value
literal = pluck $ \case
    T.Number n -> Just $ V.Number n
    T.String s -> Just $ V.String s
    TokFalse -> Just $ V.Bool False
    TokTrue -> Just $ V.Bool True
    T.Nil -> Just V.Nil
    _ -> Nothing

identifier :: Parser Text
identifier = pluck $ \case
    T.Identifier t -> Just t
    _ -> Nothing