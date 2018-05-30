{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.SimpleLabeledExprList
  ( simple_labeled_expr_list_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LabeledSimpleExpr
import Language.OCaml.Parser.Utils.Types

simple_labeled_expr_list_P :: Parser Expression -> Parser [(Arg_label, Expression)]
simple_labeled_expr_list_P seq_expr_P = many (labeled_simple_expr_P seq_expr_P)
