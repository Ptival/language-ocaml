module Language.OCaml.Parser.LblPatternList
  ( lbl_pattern_list_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LblPattern
import Language.OCaml.Parser.OptSemi
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

-- FIXME: Right now, this is made to look like the OCaml parser, but we could
-- optimize everything by factoring out the left parsers that are the same.

lbl_pattern_list_P :: Parser Pattern -> Parser ([(Loc Longident, Pattern)], Closed_flag)
lbl_pattern_list_P pattern_P = choice
  [ try $ do
    p <- lbl_pattern_P'
    semi_T
    (fields, closed) <- lbl_pattern_list_P'
    return (p : fields, closed)
  , try $ do
    p <- lbl_pattern_P'
    semi_T
    underscore_T
    opt_semi_P
    return ([p], Open)
  , try $ do
    p <- lbl_pattern_P'
    semi_T
    return ([p], Closed)
  , do
    p <- lbl_pattern_P'
    return ([p], Closed)
  ]
  where
    lbl_pattern_P'      = lbl_pattern_P      pattern_P
    lbl_pattern_list_P' = lbl_pattern_list_P pattern_P
