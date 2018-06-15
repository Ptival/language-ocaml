{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.BarConstructorDeclaration
  ( bar_constructor_declaration_P
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Attributes
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrIdent
import Language.OCaml.Parser.GeneralizedConstructorArguments
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

bar_constructor_declaration_P ::
  Parser Structure -> Parser Core_type -> Parser Constructor_declaration
bar_constructor_declaration_P structure_P core_type_P = try $ do
  bar_T
  i <- constr_ident_P
  (args, res) <- generalized_constructor_arguments_P core_type_P
  attrs <- attributes_P structure_P
  return $ constructor (def { args, attrs }) res (mkRHS i (2 :: Int))
