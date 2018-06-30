module Language.OCaml.Parser.PostItemAttributes
  ( postItemAttributesP
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.PostItemAttribute
import           Language.OCaml.Parser.Utils.Types

postItemAttributesP :: Parser Structure -> Parser [(ASTTypes.Loc String, Payload)]
postItemAttributesP structureP = many (postItemAttributeP structureP)
