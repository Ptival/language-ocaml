module Language.OCaml.Parser.TypeVariance
  ( typeVarianceP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

typeVarianceP :: Parser Variance
typeVarianceP = choice
  [ plusT  *> pure Covariant
  , minusT *> pure Contravariant
  , pure Invariant
  ]
