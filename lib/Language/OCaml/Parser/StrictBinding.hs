{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.StrictBinding
  ( strict_binding_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SeqExpr
import Language.OCaml.Parser.Tokens

strict_binding_P :: Parser Expression
strict_binding_P  = choice
  [ do
    try $ equal_T
    seq_expr_P
    -- TODO
  ]
