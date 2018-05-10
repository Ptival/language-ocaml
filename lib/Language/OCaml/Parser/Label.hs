module Language.OCaml.Parser.Label
  ( label_P
  ) where

import Text.Megaparsec.String

import Language.OCaml.Parser.Tokens

label_P :: Parser String
label_P = l_ident_T
