module Language.OCaml.Parser.SimplePatternNotIdent
  ( simple_pattern_not_ident_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.SimpleDelimitedPattern
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

simple_pattern_not_ident_P :: Parser Pattern -> Parser Pattern
simple_pattern_not_ident_P pattern_P = choice
  [ do
    underscore_T
    return $ mkpat $ Ppat_any
  -- TODO: signed constants
  , do
    i <- constr_longident_P
    return $ mkpat $ Ppat_construct (mkRHS i 1) Nothing
  -- TODO: name_tag
  -- TODO: HASH
  , simple_delimited_pattern_P'
  , do
    l_paren_T
    p <- pattern_P
    r_paren_T
    return $ reloc_pat p
  ]
  where
    simple_delimited_pattern_P' = simple_delimited_pattern_P pattern_P
