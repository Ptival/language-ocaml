{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.BarConstructorDeclaration
  ( barConstructorDeclarationP
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTHelper.Type     as Type
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Attributes
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrIdent
import Language.OCaml.Parser.GeneralizedConstructorArguments
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

barConstructorDeclarationP ::
  Parser Structure -> Parser CoreType -> Parser ConstructorDeclaration
barConstructorDeclarationP structureP coreTypeP = try $ do
  barT
  i <- constrIdentP
  (args, res) <- generalizedConstructorArgumentsP coreTypeP
  attrs <- attributesP structureP
  return $ Type.constructor (def { args, attrs }) res (mkRHS i (2 :: Int))
