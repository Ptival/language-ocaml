module Language.OCaml.Parser.LblExprList
  ( lbl_expr_list_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LblExpr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

lbl_expr_list_P :: Parser Expression -> Parser [(Loc Longident, Expression)]
lbl_expr_list_P expr_P = choice
  [ do
    e <- lbl_expr_P'
    semi_T
    l <- lbl_expr_list_P'
    return $ e : l
  , do
    e <- lbl_expr_P'
    semi_T
    return [e]
  , (: []) <$> lbl_expr_P'
  ]
  where
    lbl_expr_P'      = lbl_expr_P      expr_P
    lbl_expr_list_P' = lbl_expr_list_P expr_P
