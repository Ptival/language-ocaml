{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.PatternNoExn
  ( pattern_no_exn_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.PatternGen
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Combinators

pattern_no_exn_P :: Parser Pattern
pattern_no_exn_P = leftRecursive
  [ pattern_gen_P pattern_P
  ]
  [ do
    try as_T
    i <- val_ident_P
    return $ \ x -> mkpat $ Ppat_alias x (mkRHS i 3)
  -- TODO: pattern_no_exn_comma_list
  -- , do
  --   try colon_colon_T
  --   p <- pattern_P
  --   return $ mkpat_cons _ _
  , do
    try bar_T
    p <- pattern_P
    return $ \ x -> mkpat $ Ppat_or x p
  -- TODO: attribute
  ]
