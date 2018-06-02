module Language.OCaml.Parser.OptionalTypeParameters
  ( optional_type_parameters_P
  ) where

import           Text.Megaparsec

import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.OptionalTypeParameter
import           Language.OCaml.Parser.Utils.Types
import           Language.OCaml.Parser.Utils.Utils

optional_type_parameters_P :: Parser [(Core_type, Variance)]
optional_type_parameters_P = choice
  [ (: []) <$> optional_type_parameter_P
  , parens (many optional_type_parameter_P)
  , pure []
  ]
