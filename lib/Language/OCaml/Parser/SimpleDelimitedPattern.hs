module Language.OCaml.Parser.SimpleDelimitedPattern
  ( simple_delimited_pattern_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LblPatternList
import Language.OCaml.Parser.OptSemi
import Language.OCaml.Parser.PatternSemiList
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

simple_delimited_pattern_P :: Parser Pattern -> Parser Pattern
simple_delimited_pattern_P pattern_P = choice
  [ do
    (fields, closed) <- try $ do
      l_brace_T
      lbl_pattern_list_P pattern_P
    r_brace_T
    return $ mkpat $ Ppat_record fields closed
  , do
    l <- try $ do
      l_bracket_T
      l <- pattern_semi_list_P pattern_P
      opt_semi_P
      return l
    r_bracket_T
    return $ mktailpat (rhsLoc 4) (reverse l)
  , do
    l <- try $ do
      l_bracket_bar_T
      l <- pattern_semi_list_P'
      opt_semi_P
      return l
    bar_r_bracket_T
    return $ mkpat $ Ppat_array (reverse l)
  , do
    l_bracket_bar_T
    bar_r_bracket_T
    return $ mkpat $ Ppat_array []
  ]
  where
    pattern_semi_list_P' = pattern_semi_list_P pattern_P
