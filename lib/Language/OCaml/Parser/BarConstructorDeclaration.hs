{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.BarConstructorDeclaration
  ( bar_constructor_declaration_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
-- import Language.OCaml.Parser.Attributes
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrIdent
import Language.OCaml.Parser.GeneralizedConstructorArguments
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

bar_constructor_declaration_P :: Parser Core_type -> Parser Constructor_declaration
bar_constructor_declaration_P core_type_P = try $ do
  bar_T
  i <- constr_ident_P
  (args, res) <- generalized_constructor_arguments_P core_type_P
  -- TODO: attributes
  return $ constructor args res (mkRHS i 2)
