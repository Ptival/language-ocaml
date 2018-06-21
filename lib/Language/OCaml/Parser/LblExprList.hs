module Language.OCaml.Parser.LblExprList
  ( lbl_expr_list_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LblExpr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

-- FIXME: Right now, this is made to look like the OCaml parser, but we could
-- optimize everything by factoring out the left parsers that are the same.

lbl_expr_list_P :: Parser Expression -> Parser [(Loc Longident, Expression)]
lbl_expr_list_P expr_P = choice
  [ try $ do
    e <- lbl_expr_P'
    semi_T
    l <- lbl_expr_list_P'
    return $ e : l
  , try $ do
    e <- lbl_expr_P'
    semi_T
    return [e]
  , (: []) <$> lbl_expr_P'
  ]
  where
    lbl_expr_P'      = lbl_expr_P      expr_P
    lbl_expr_list_P' = lbl_expr_list_P expr_P
