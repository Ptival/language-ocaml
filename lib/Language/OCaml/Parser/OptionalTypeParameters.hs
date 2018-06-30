module Language.OCaml.Parser.OptionalTypeParameters
  ( optionalTypeParametersP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.OptionalTypeParameter
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.Parser.Utils.Utils

optionalTypeParametersP :: Parser [(CoreType, Variance)]
optionalTypeParametersP = choice
  [ (: []) <$> optionalTypeParameterP
  , parens (many optionalTypeParameterP)
  , pure []
  ]
