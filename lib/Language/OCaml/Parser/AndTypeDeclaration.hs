{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.AndTypeDeclaration
  ( andTypeDeclarationP
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTHelper.Type as Type
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TypeKind
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Utils.Types

andTypeDeclarationP :: Parser Structure -> Parser TypeDeclaration
andTypeDeclarationP structureP = do
  try $ andT
  -- TODO: attributes
  -- TODO: optionalTypeParameters
  i <- lIdentT
  (kind, priv, manifest) <- typeKindP structureP
  -- TODO: constraints
  attrs <- postItemAttributesP structureP -- FIXME: use me
  return $ Type.mk (def { attrs, kind, priv }) manifest (mkRHS i 4) -- FIXME: [] and []
