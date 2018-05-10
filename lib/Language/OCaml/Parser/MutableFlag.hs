module Language.OCaml.Parser.MutableFlag
  ( mutable_flag_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens

mutable_flag_P :: Parser Mutable_flag
mutable_flag_P = choice
  [ mutable_T *> return Mutable
  , return Immutable
  ]
