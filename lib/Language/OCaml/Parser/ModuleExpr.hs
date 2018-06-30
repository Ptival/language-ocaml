module Language.OCaml.Parser.ModuleExpr
  ( moduleExprP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Utils.Types

moduleExprP :: Parser ModuleExpr
moduleExprP = choice
  [ do
    i <- modLongidentP
    return . mkmod Nothing $ PmodIdent (mkRHS i 1)
  ]
