{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.Expr
  ( expr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.MatchCases
import Language.OCaml.Parser.OptBar
import Language.OCaml.Parser.SimpleLabeledExprList
import Language.OCaml.Parser.SimpleExpr
import Language.OCaml.Parser.Tokens

expr_P :: Parser Expression -> Parser Expression
expr_P seq_expr_P = choice
  [ do
    e <- simple_expr_P
    l <- simple_labeled_expr_list_P
    return $ mkexp $ Pexp_apply e (reverse l)
  , simple_expr_P
  , do
    try $ function_T
    -- TODO: ext_attributes
    opt_bar_P
    l <- match_cases_P seq_expr_P
    return $ mkexp_attrs (Pexp_function (reverse l)) (Nothing, []) -- FIXME
  ]
