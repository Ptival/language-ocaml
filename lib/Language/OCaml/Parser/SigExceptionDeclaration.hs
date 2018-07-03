{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.SigExceptionDeclaration
  ( sigExceptionDeclarationP
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Te as Te
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrIdent
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.GeneralizedConstructorArguments
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

sigExceptionDeclarationP :: Parser Structure -> Parser (ExtensionConstructor, ())
sigExceptionDeclarationP structureP = do
  exceptionT
  -- TODO: extAttributes
  i <- constrIdentP
  (args, res) <- generalizedConstructorArgumentsP coreTypeP
  -- a <- attributesP
  attrs <- postItemAttributesP structureP
  return
    ( Te.decl
      (def { args
           , attrs = attrs {- ++ a -} {- loc and docs -}
           , loc   = symbolRLoc ()
           , docs  = symbolDocs ()
           }
      )
      res
      (mkRHS i (3 :: Int))
    , ()
    )
