module Language.OCaml.Parser.LblExpr
  ( lblExprP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabelLongident
import Language.OCaml.Parser.OptTypeConstraint
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

lblExprP :: Parser Expression -> Parser (Loc Longident, Expression)
lblExprP exprP = choice
  [ do
    i <- labelLongidentP
    c <- optTypeConstraintP
    equalT
    e <- exprP
    return (mkRHS i 1, mkexpOptConstraint e c)
  ]
