module Language.OCaml.Parser.ConstructorArguments
  ( constructor_arguments_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeList
import Language.OCaml.Parser.Utils.Types

constructor_arguments_P :: Parser Core_type -> Parser Constructor_arguments
constructor_arguments_P core_type_P = choice
  [ Pcstr_tuple . reverse <$> core_type_list_P core_type_P
  -- TODO: label declarations
  ]
