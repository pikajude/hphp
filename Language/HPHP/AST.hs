module Language.HPHP.AST (
    PHP(..),
    Expr(..)
) where

import Data.Text

data PHP = Expr Expr
         deriving Show

data Expr = Var Expr
          | StringLiteral Text
          | Concat Expr Expr
          deriving Show
