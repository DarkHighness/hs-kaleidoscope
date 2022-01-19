{-# LANGUAGE OverloadedStrings #-}

module Lang.Lexer where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, choice, many, optional, sepBy, try)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as CL

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  CL.space
    space1
    (CL.skipLineComment "//")
    (CL.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = CL.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = CL.symbol spaceConsumer

integer :: Parser Integer
integer =
  lexeme
    ( choice
        [ try (char '0' >> char 'x' >> CL.hexadecimal),
          try (char '0' >> char 'o' >> CL.octal),
          try (char '0' >> char 'b' >> CL.binary),
          CL.decimal
        ]
    )

signedInteger :: Parser Integer
signedInteger = CL.signed spaceConsumer integer

float :: Parser Double
float =
  lexeme CL.float

signedFloat :: Parser Double
signedFloat = CL.signed spaceConsumer float

identifier :: Parser Text
identifier = do
  h <- choice [C.letterChar, char '$']
  b <- many C.alphaNumChar
  t <- many (char '\'')
  (return . T.pack) (h : b ++ t)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

semiSep :: Parser a -> Parser [a]
semiSep p = p `sepBy` char ';'

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` char ','

reversed :: Parser Text
reversed =
  choice $
    symbol
      <$> [ "def",
            "extern"
          ]

reversedOp :: Parser Text
reversedOp =
  choice $
    symbol <$> ["+", "*", "-", "/"]