module Language.OCaml.Parser.MutableFlag
  ( mutable_flag_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

mutable_flag_P :: Parser Mutable_flag
mutable_flag_P = choice
  [ mutable_T *> return Mutable
  , return Immutable
  ]
