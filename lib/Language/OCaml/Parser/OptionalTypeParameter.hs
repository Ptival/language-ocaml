module Language.OCaml.Parser.OptionalTypeParameter
  ( optional_type_parameter_P
  ) where

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.OptionalTypeVariable
import Language.OCaml.Parser.TypeVariance
import Language.OCaml.Parser.Utils.Types

optional_type_parameter_P :: Parser (Core_type, Variance)
optional_type_parameter_P = do
  v <- type_variance_P
  t <- optional_type_variable_P
  return (t, v)
