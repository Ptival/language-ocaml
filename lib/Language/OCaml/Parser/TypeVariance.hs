module Language.OCaml.Parser.TypeVariance
  ( type_variance_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

type_variance_P :: Parser Variance
type_variance_P = choice
  [ plus_T  *> pure Covariant
  , minus_T *> pure Contravariant
  , pure Invariant
  ]
