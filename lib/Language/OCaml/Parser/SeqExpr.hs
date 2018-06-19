module Language.OCaml.Parser.SeqExpr
  ( seq_expr_P
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Expr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

seq_expr_P :: Parser Structure -> Parser Expression
seq_expr_P structure_P = choice
  [ try $ do
    e <- expr_P'
    semi_T
    s <- seq_expr_P'
    return $ mkExp def (Pexp_sequence e s)
  , try $ do
    e <- expr_P'
    semi_T
    return e
  , try $ expr_P'
  -- TODO: percent
  ]
  where
    expr_P' = expr_P structure_P seq_expr_P'
    seq_expr_P' = seq_expr_P structure_P
