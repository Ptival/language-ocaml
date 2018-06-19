{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.SigExceptionDeclaration
  ( sig_exception_declaration_P
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

sig_exception_declaration_P :: Parser Structure -> Parser (TypeException, ())
sig_exception_declaration_P structure_P = do
  exception_T
  -- TODO: ext_attributes
  i <- constr_ident_P
  (args, res) <- generalized_constructor_arguments_P core_type_P
  -- a <- attributes_P
  attrs <- post_item_attributes_P structure_P
  return $ ( mk_exception (def { attrs })
             (decl (def { args, attrs = attrs {- ++ a -} {- loc and docs -} })
              res (mkRHS i (3 :: Int)))
           , ()
           )
