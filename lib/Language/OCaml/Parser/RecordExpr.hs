module Language.OCaml.Parser.RecordExpr
  ( record_expr_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LblExprList
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

record_expr_P ::
  Parser Expression -> Parser Expression -> Parser (Maybe Expression, [(Loc Longident, Expression)])
record_expr_P expr_P simple_expr_P = choice
  [ do
    e <- simple_expr_P
    with_T
    l <- lbl_expr_list_P expr_P
    return (Just e, l)
  ]
