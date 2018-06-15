{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.ConstructorDeclaration
  ( constructor_declaration_P
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrIdent
import Language.OCaml.Parser.GeneralizedConstructorArguments
import Language.OCaml.Parser.Utils.Types

constructor_declaration_P :: Parser Core_type -> Parser Constructor_declaration
constructor_declaration_P core_type_P = do
  name <- constr_ident_P
  (args, res) <- generalized_constructor_arguments_P core_type_P
  -- attributes_P
  return $ constructor args res (mkRHS name (1 :: Int))
