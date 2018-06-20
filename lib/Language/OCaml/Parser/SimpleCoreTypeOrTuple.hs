module Language.OCaml.Parser.SimpleCoreTypeOrTuple
  ( simple_core_type_or_tuple_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.CoreTypeList
import Language.OCaml.Parser.SimpleCoreType
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

simple_core_type_or_tuple_P :: Parser Core_type -> Parser Core_type
simple_core_type_or_tuple_P core_type_P = choice
  [ do
    t <- try $ do
      t <- simple_core_type_P'
      star_T
      return t
    l <- core_type_list_P core_type_P
    return . mktyp . Ptyp_tuple $ t : reverse l
  , simple_core_type_P'
  ]
  where
    simple_core_type_P' = simple_core_type_P core_type_P
