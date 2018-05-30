module Language.OCaml.Parser.FunBinding
  ( fun_binding_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.TypeConstraint
import Language.OCaml.Parser.StrictBinding
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

fun_binding_P :: Parser Expression -> Parser Expression
fun_binding_P seq_expr_P = choice
  [ strict_binding_P seq_expr_P (fun_binding_P seq_expr_P)
  , do
    c <- type_constraint_P
    equal_T
    e <- seq_expr_P
    return $ mkexp_constraint e c
  ]
