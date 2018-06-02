module Language.OCaml.Parser.OptionalTypeVariable
  ( optional_type_variable_P
  ) where


import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

optional_type_variable_P :: Parser Core_type
optional_type_variable_P = mktyp . Ptyp_var <$> (quote_T *> ident_P)
