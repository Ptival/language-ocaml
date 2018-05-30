{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.AndTypeDeclaration
  ( and_type_declaration_P
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TypeKind
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Utils.Types

and_type_declaration_P :: Parser Structure -> Parser Type_declaration
and_type_declaration_P structure_P = do
  try $ and_T
  -- TODO: attributes
  -- TODO: optional_type_parameters
  i <- l_ident_T
  (kind, priv, manifest) <- type_kind_P
  -- TODO: constraints
  attrs <- post_item_attributes_P structure_P -- FIXME: use me
  return $ mkType (def { attrs, kind, priv }) manifest (mkRHS i 4) -- FIXME: [] and []
