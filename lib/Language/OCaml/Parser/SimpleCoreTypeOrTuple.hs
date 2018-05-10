module Language.OCaml.Parser.SimpleCoreTypeOrTuple
  ( simple_core_type_or_tuple_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreType

simple_core_type_or_tuple_P :: Parser Core_type
simple_core_type_or_tuple_P = choice
  [ simple_core_type_P
  -- TODO
  ]
