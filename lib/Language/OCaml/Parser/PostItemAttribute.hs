module Language.OCaml.Parser.PostItemAttribute
  ( post_item_attribute_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.AttrId
import           Language.OCaml.Parser.Payload
import           Language.OCaml.Parser.Tokens

post_item_attribute_P :: Parser Structure -> Parser (ASTTypes.Loc String, Payload)
post_item_attribute_P structure_P = do
  try $ l_bracket_at_at_T
  i <- attr_id_P
  p <- payload_P structure_P
  r_bracket_T
  return (i, p)
