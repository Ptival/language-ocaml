module Language.OCaml.Parser.ModLongident
  ( mod_longident_P
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

mod_longident_P :: Parser Longident
mod_longident_P = chainl1try' u_ident_T (dot_T *> return Ldot) Lident
