{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.CoreTypeList
  ( core_type_list_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreType
import Language.OCaml.Parser.Tokens

core_type_list_P :: Parser [Core_type]
core_type_list_P = simple_core_type_P `sepBy` star_T
