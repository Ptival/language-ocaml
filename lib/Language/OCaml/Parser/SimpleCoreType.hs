module Language.OCaml.Parser.SimpleCoreType
  ( simple_core_type_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeCommaList
import Language.OCaml.Parser.SimpleCoreType2
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.Parser.Utils.Utils

simple_core_type_P :: Parser Core_type -> Parser Core_type
simple_core_type_P core_type_P = choice
  [ try $ simple_core_type2_P core_type_P
  , do
    l <- parens $ core_type_comma_list_P core_type_P
    case l of
      [sty] -> return sty
      _ -> fail "Parse error"
  ]
