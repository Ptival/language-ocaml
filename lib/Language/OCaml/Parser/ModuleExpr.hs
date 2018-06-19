module Language.OCaml.Parser.ModuleExpr
  ( module_expr_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Utils.Types

module_expr_P :: Parser Module_expr
module_expr_P = choice
  [ do
    i <- mod_longident_P
    return . mkmod Nothing $ Pmod_ident (mkRHS i 1)
  ]
