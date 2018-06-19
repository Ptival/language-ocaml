module Language.OCaml.Parser.SimplePattern
  ( simple_pattern_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.SimplePatternNotIdent
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Types

simple_pattern_P :: Parser Pattern -> Parser Pattern
simple_pattern_P pattern_P = choice
  [ do
    i <- val_ident_P
    return $ mkpat $ Ppat_var $ mkRHS i 1
  , simple_pattern_not_ident_P pattern_P
  ]
