module Language.OCaml.Parser.OptTypeConstraint
  ( opt_type_constraint_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.TypeConstraint
import Language.OCaml.Parser.Utils.Types

opt_type_constraint_P :: Parser (Maybe (Maybe Core_type, Maybe Core_type))
opt_type_constraint_P = choice
  [ Just <$> type_constraint_P
  , return Nothing
  ]
