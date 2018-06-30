module Language.OCaml.Parser.TypeParameter
  ( typeParameterP
  ) where

import           Data.Default
import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.NonrecFlag
import           Language.OCaml.Parser.PostItemAttributes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.TypeKind
import           Language.OCaml.Parser.TypeVariable
import           Language.OCaml.Parser.TypeVariance
import           Language.OCaml.Parser.Utils.Types

typeParameterP :: Parser [a]
typeParameterP = do
  v <- typeVarianceP
  t <- typeVariableP
  return (v, t)
