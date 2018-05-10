{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.SimpleLabeledExprList
  ( simple_labeled_expr_list_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LabeledSimpleExpr

simple_labeled_expr_list_P :: Parser [(Arg_label, Expression)]
simple_labeled_expr_list_P = many labeled_simple_expr_P
