module Language.OCaml.Parser.ConstructorArguments
  ( constructor_arguments_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeList

constructor_arguments_P :: Parser Constructor_arguments
constructor_arguments_P = choice
  [ Pcstr_tuple . reverse <$> core_type_list_P
  -- TODO: label declarations
  ]
