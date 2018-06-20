module Language.OCaml.Parser.CoreTypeCommaList
  ( core_type_comma_list_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

core_type_comma_list_P :: Parser Core_type -> Parser [Core_type]
core_type_comma_list_P core_type_P = sepBy core_type_P comma_T
