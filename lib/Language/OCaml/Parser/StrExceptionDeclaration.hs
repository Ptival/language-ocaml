{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.StrExceptionDeclaration
  ( str_exception_declaration_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SigExceptionDeclaration
import Language.OCaml.Parser.Utils.Types

str_exception_declaration_P :: Parser Structure -> Parser (TypeException, ())
str_exception_declaration_P structure_P = choice
  [ sig_exception_declaration_P structure_P
    -- TODO
  ]
