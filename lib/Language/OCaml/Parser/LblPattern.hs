module Language.OCaml.Parser.LblPattern
  ( lblPatternP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabelLongident
import Language.OCaml.Parser.OptPatternTypeConstraint
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

lblPatternP :: Parser Pattern -> Parser (Loc Longident, Pattern)
lblPatternP patternP = choice
  [ do
    (i, c) <- try $ do
      i <- labelLongidentP
      c <- optPatternTypeConstraintP
      equalT
      return (i, c)
    p <- patternP
    return (mkRHS i 1, mkpatOptConstraint p c)
  , do
    i <- labelLongidentP
    c <- optPatternTypeConstraintP
    return (mkRHS i 1, mkpatOptConstraint (patOfLabel i 1) c)
  ]
