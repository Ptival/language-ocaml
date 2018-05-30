{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.SimplePatternNotIdent
  ( simple_pattern_not_ident_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

simple_pattern_not_ident_P :: Parser Pattern
simple_pattern_not_ident_P = choice
  [ do
    underscore_T
    return $ mkpat $ Ppat_any
  -- TODO: signed constants
  , do
    i <- constr_longident_P
    return $ mkpat $ Ppat_construct (mkRHS i 1) Nothing
  ]
