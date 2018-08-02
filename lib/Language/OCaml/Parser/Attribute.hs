module Language.OCaml.Parser.Attribute
  ( attributeP
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes  as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.AttrId
import           Language.OCaml.Parser.Payload
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

attributeP :: Parser Structure -> Parser (ASTTypes.Loc String, Payload)
attributeP structureP = do
  i <- try $ do
    lBracketAtT
    attrIdP
  p <- payloadP structureP
  rBracketT
  return (i, p)
