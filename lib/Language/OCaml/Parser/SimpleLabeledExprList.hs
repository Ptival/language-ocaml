{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.SimpleLabeledExprList
  ( simple_labeled_expr_list_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LabeledSimpleExpr
import Language.OCaml.Parser.Utils.Types

-- NOTE: the OCaml version builds the list backwards for efficiency reasons
-- We call `reverse` to match it, even though it is inefficient.
-- We could update this once the whole code base is ready. #FIXME
simple_labeled_expr_list_P :: Parser Expression -> Parser [(Arg_label, Expression)]
simple_labeled_expr_list_P seq_expr_P = reverse <$> many (labeled_simple_expr_P seq_expr_P)
