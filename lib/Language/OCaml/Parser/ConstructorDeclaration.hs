{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.ConstructorDeclaration
  ( constructorDeclarationP
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Type as Type
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Attributes
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrIdent
import Language.OCaml.Parser.GeneralizedConstructorArguments
import Language.OCaml.Parser.Utils.Types

constructorDeclarationP ::
  Parser Structure -> Parser CoreType -> Parser ConstructorDeclaration
constructorDeclarationP structureP coreTypeP = do
  name <- constrIdentP
  (args, res) <- generalizedConstructorArgumentsP coreTypeP
  attrs <- attributesP structureP
  return $ Type.constructor (def { args, attrs }) res (mkRHS name (1 :: Int))
