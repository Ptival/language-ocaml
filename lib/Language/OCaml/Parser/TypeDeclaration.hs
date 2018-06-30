{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.TypeDeclaration
  ( typeDeclarationP
  ) where

import           Data.Default
import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.NonRecFlag
import           Language.OCaml.Parser.OptionalTypeParameters
import           Language.OCaml.Parser.PostItemAttributes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.TypeKind
import           Language.OCaml.Parser.Utils.Types

typeDeclarationP :: Parser Structure -> Parser (ASTTypes.RecFlag, TypeDeclaration)
typeDeclarationP structureP = do
  try $ typeT
  -- TODO: extAttributes
  nonrecFlag <- nonrecFlagP
  params <- optionalTypeParametersP
  n <- lIdentT
  (kind, priv, manifest) <- typeKindP structureP
  -- TODO: constraints
  attrs <- postItemAttributesP structureP
  let ty = mkType (def { attrs, kind, params, priv }) manifest (mkRHS n 5)
  return (nonrecFlag, ty)
