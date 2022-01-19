{-# LANGUAGE OverloadedStrings #-}

module Lang.Lexer where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, try)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

integer :: Parser Integer
integer =
  lexeme
    ( choice
        [ try (char '0' >> char 'x' >> L.hexadecimal),
          try (char '0' >> char 'o' >> L.octal),
          try (char '0' >> char 'b' >> L.binary),
          L.decimal
        ]
    )