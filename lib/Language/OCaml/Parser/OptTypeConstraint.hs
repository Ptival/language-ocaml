module Language.OCaml.Parser.OptTypeConstraint
  ( optTypeConstraintP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.TypeConstraint
import Language.OCaml.Parser.Utils.Types

optTypeConstraintP :: Parser (Maybe (Maybe CoreType, Maybe CoreType))
optTypeConstraintP = choice
  [ Just <$> typeConstraintP
  , return Nothing
  ]
