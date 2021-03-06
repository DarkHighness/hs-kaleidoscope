{-# LANGUAGE OverloadedStrings #-}

module Lang.Parser where

import Control.Monad.Combinators.Expr (Operator (InfixL, Postfix, Prefix), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Lang.Lexer
import Lang.Syntax
import Text.Megaparsec (MonadParsec (eof, try), ParseError, ParseErrorBundle, choice, many, parse)
import Text.Megaparsec.Char (space, space1)
import Text.Megaparsec.Debug (dbg)

pVar :: Parser Expr
pVar = Var <$> identifier

int :: Parser Expr
int = Float . fromInteger <$> integer

floating :: Parser Expr
floating =
  Float
    <$> float

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

term :: Parser Expr
term =
  choice
    [ try floating,
      try int,
      try extern,
      try function,
      try call,
      try variable,
      parens expr
    ]

defn :: Parser Expr
defn =
  choice
    [ try extern,
      try function,
      expr
    ]

contents :: Parser a -> Parser a
contents p = do
  space
  r <- p
  eof
  return r

topLevel :: Parser [Expr]
topLevel = many $ do
  def <- defn
  reservedOp ";"
  return def

expr :: Parser Expr
expr = makeExprParser term operatorTable

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" (UnOp Negation),
      prefix "+" id
    ],
    [ binary "*" (BinOp Times),
      binary "/" (BinOp Divide)
    ],
    [ binary "+" (BinOp Plus),
      binary "-" (BinOp Minus)
    ],
    [ binary "<" (BinOp LessThan)
    ],
    [ binary "=" (BinOp Equals)
    ]
  ]

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (contents expr) "<stdin>"

parseTopLevel :: Text -> Either (ParseErrorBundle Text Void) [Expr]
parseTopLevel = parse (contents topLevel) "<stdin>"