module Language.OCaml.Parser.PostItemAttribute
  ( postItemAttributeP
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes  as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.AttrId
import           Language.OCaml.Parser.Payload
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

postItemAttributeP :: Parser Structure -> Parser (ASTTypes.Loc String, Payload)
postItemAttributeP structureP = do
  try $ lBracketAtAtT
  i <- attrIdP
  p <- payloadP structureP
  rBracketT
  return (i, p)
