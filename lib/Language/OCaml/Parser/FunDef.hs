module Language.OCaml.Parser.FunDef
  ( fun_def_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabeledSimplePattern
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.PrettyPrinter ()

fun_def_P :: Parser Expression -> Parser Expression
fun_def_P seq_expr_P = choice
  [ do
    minus_greater_T
    seq_expr_P
  -- TODO
  , do
    (l, o, p) <- labeled_simple_pattern_P pattern_P
    d <- fun_def_P'
    return $ ghexp $ Pexp_fun l o p d
  ]
  where
    fun_def_P' = fun_def_P seq_expr_P
