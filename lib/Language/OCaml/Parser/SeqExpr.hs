{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.SeqExpr
  ( seq_expr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Expr
import Language.OCaml.Parser.Tokens

seq_expr_P :: Parser Expression
seq_expr_P = choice
  [ try $ do
    e <- expr_P seq_expr_P
    semi_T
    s <- seq_expr_P
    return $ mkExp Nothing Nothing (Pexp_sequence e s)
  , try $ do
    e <- expr_P seq_expr_P
    semi_T
    return e
  , expr_P seq_expr_P
  -- TODO: percent
  ]
