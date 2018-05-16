module Language.OCaml.Parser.Operator
  ( operator_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Parser.Tokens

operator_P :: Parser String
operator_P = choice
  [
    -- TODO
    bang_T  *> return "!"
  , plus_T  *> return "+"
  , minus_T *> return "-"
  ]
