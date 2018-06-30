module Language.OCaml.Parser.LabeledSimplePattern
  ( labeledSimplePatternP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimplePattern
import Language.OCaml.Parser.Utils.Types

labeledSimplePatternP :: Parser Pattern -> Parser (ArgLabel, Maybe a, Pattern)
labeledSimplePatternP patternP = choice
  [ -- TODO: lots of things
    do
      p <- simplePatternP patternP
      return (Nolabel, Nothing, p)
  ]
