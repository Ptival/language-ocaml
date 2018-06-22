module Language.OCaml.Parser.OptPatternTypeConstraint
  ( opt_pattern_type_constraint_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

opt_pattern_type_constraint_P :: Parser (Maybe Core_type)
opt_pattern_type_constraint_P = choice
  [ Just <$> (colon_T *> core_type_P)
  , return Nothing
  ]
