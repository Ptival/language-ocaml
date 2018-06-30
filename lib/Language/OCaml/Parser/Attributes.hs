module Language.OCaml.Parser.Attributes
  ( attributesP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Attribute
import Language.OCaml.Parser.Utils.Types

attributesP :: Parser Structure -> Parser [(Loc String, Payload)]
attributesP structureP = many (attributeP structureP)
