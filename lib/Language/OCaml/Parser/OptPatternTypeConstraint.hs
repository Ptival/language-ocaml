module Language.OCaml.Parser.OptPatternTypeConstraint
  ( optPatternTypeConstraintP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

optPatternTypeConstraintP :: Parser (Maybe CoreType)
optPatternTypeConstraintP = choice
  [ Just <$> (colonT *> coreTypeP)
  , return Nothing
  ]
