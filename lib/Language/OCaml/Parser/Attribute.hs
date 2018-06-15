module Language.OCaml.Parser.Attribute
  ( attribute_P
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.AttrId
import           Language.OCaml.Parser.Payload
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

attribute_P :: Parser Structure -> Parser (ASTTypes.Loc String, Payload)
attribute_P structure_P = do
  try $ l_bracket_at_T
  i <- attr_id_P
  p <- payload_P structure_P
  r_bracket_T
  return (i, p)
