{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.Pattern
  ( pattern_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.PatternCommaList
import Language.OCaml.Parser.PatternGen
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Combinators

pattern_P :: Parser Pattern
pattern_P = leftRecursive
  [ pattern_gen_P pattern_P
  -- , do
  --   l <- pattern_comma_list_P pattern_P
  --   return $ mkpat $ Ppat_tuple (reverse l)
  ]
  [
    do
    try $ as_T
    i <- val_ident_P
    return $ \ x -> mkpat $ Ppat_alias x (mkRHS i 3)
  -- TODO: pattern COLONCOLON pattern
  , do
    try $ bar_T
    p <- pattern_P
    return $ \ x -> mkpat $ Ppat_or x p
  -- TODO: bunch of other patterns
  ]
