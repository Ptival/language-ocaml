module Language.OCaml.Parser.SimpleCoreTypeOrTuple
  ( simple_core_type_or_tuple_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreType
import Language.OCaml.Parser.Utils.Types

simple_core_type_or_tuple_P :: Parser Core_type
simple_core_type_or_tuple_P = choice
  [ simple_core_type_P
  -- TODO
  ]
