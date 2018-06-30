{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.SigExceptionDeclaration
  ( sigExceptionDeclarationP
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrIdent
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.GeneralizedConstructorArguments
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

sigExceptionDeclarationP :: Parser Structure -> Parser (TypeException, ())
sigExceptionDeclarationP structureP = do
  exceptionT
  -- TODO: extAttributes
  i <- constrIdentP
  (args, res) <- generalizedConstructorArgumentsP coreTypeP
  -- a <- attributesP
  attrs <- postItemAttributesP structureP
  return $ ( mkException (def { attrs })
             (decl
              (def { args, attrs = attrs {- ++ a -} {- loc and docs -} })
              res
              (mkRHS i (3 :: Int))
             )
           , ()
           )
