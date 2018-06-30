module Language.OCaml.Parser.OptionalTypeParameter
  ( optionalTypeParameterP
  ) where

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.OptionalTypeVariable
import Language.OCaml.Parser.TypeVariance
import Language.OCaml.Parser.Utils.Types

optionalTypeParameterP :: Parser (CoreType, Variance)
optionalTypeParameterP = do
  v <- typeVarianceP
  t <- optionalTypeVariableP
  return (t, v)
