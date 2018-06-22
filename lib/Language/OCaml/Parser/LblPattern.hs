module Language.OCaml.Parser.LblPattern
  ( lbl_pattern_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabelLongident
import Language.OCaml.Parser.OptPatternTypeConstraint
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

lbl_pattern_P :: Parser Pattern -> Parser (Loc Longident, Pattern)
lbl_pattern_P pattern_P = choice
  [ do
    (i, c) <- try $ do
      i <- label_longident_P
      c <- opt_pattern_type_constraint_P
      equal_T
      return (i, c)
    p <- pattern_P
    return (mkRHS i 1, mkpat_opt_constraint p c)
  , do
    i <- label_longident_P
    c <- opt_pattern_type_constraint_P
    return (mkRHS i 1, mkpat_opt_constraint (pat_of_label i 1) c)
  ]
