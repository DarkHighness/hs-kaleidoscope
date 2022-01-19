module Lang.Syntax where

import Data.Text (Text)

type Name = Text

data Expr
  = Float Double
  | BinOp Op Expr Expr
  | Var Text
  | Call Name [Expr]
  | Function Name [Expr] Expr
  | Extern Name [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)