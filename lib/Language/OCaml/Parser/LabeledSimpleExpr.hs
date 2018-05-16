{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.LabeledSimpleExpr
  ( labeled_simple_expr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleExpr

labeled_simple_expr_P :: Parser Expression -> Parser (Arg_label, Expression)
labeled_simple_expr_P seq_expr_P = choice
  [ do
    e <- simple_expr_P seq_expr_P
    return (Nolabel, e)
    -- TODO: label_expr
  ]
