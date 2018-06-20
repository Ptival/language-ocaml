module Language.OCaml.Parser.LblExpr
  ( lbl_expr_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabelLongident
import Language.OCaml.Parser.OptTypeConstraint
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

lbl_expr_P :: Parser Expression -> Parser (Loc Longident, Expression)
lbl_expr_P expr_P = choice
  [ do
    i <- label_longident_P
    c <- opt_type_constraint_P
    equal_T
    e <- expr_P
    return (mkRHS i 1, mkexp_opt_constraint e c)
  ]
