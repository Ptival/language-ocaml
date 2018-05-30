module Language.OCaml.Parser.Label
  ( label_P
  ) where

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

label_P :: Parser String
label_P = l_ident_T
