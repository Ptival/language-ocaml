{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.StrictBinding
  ( strict_binding_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabeledSimplePattern
import Language.OCaml.Parser.SeqExpr
import Language.OCaml.Parser.Tokens

strict_binding_P :: Parser Expression -> Parser Expression
strict_binding_P fun_binding_P = choice
  [ do
    try $ equal_T
    seq_expr_P
  , do
    (l, o, p) <- labeled_simple_pattern_P
    b <- fun_binding_P
    return $ ghexp $ Pexp_fun l o p b
    -- TODO
  ]
