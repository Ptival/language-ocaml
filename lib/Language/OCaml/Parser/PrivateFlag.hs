module Language.OCaml.Parser.PrivateFlag
  ( private_flag_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

private_flag_P :: Parser Private_flag
private_flag_P = choice
  [ private_T *> return Private
  , return Public
  ]
