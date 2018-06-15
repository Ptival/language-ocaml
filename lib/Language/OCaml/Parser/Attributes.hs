module Language.OCaml.Parser.Attributes
  ( attributes_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Attribute
import Language.OCaml.Parser.Utils.Types

attributes_P :: Parser Structure -> Parser [(Loc String, Payload)]
attributes_P structure_P = many (attribute_P structure_P)
