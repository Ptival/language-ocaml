{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.SimplePattern
  ( simple_pattern_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.SimplePatternNotIdent
import Language.OCaml.Parser.ValIdent

simple_pattern_P :: Parser Pattern
simple_pattern_P = choice
  [ do
    i <- val_ident_P
    return $ mkpat $ Ppat_var $ mkRHS i 1
  , simple_pattern_not_ident_P
  ]
