module Lang.Syntax where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Name = Text

type Parser = Parsec Void Text

data Expr
  = Float Double
  | BinOp BOp Expr Expr
  | UnOp UOp Expr
  | Var Text
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  deriving (Eq, Ord, Show)

data BOp
  = Plus
  | Minus
  | Times
  | Divide
  | Equals
  | LessThan
  deriving (Eq, Ord, Show)

data UOp
  = Negation
  deriving (Show, Ord, Eq)