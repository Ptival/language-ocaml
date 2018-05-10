{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.PostItemAttributes
  ( post_item_attributes_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.PostItemAttribute

post_item_attributes_P :: Parser Structure -> Parser [(ASTTypes.Loc String, Payload)]
post_item_attributes_P structure_P = many (post_item_attribute_P structure_P)
