module Language.OCaml.Parser.Operator
  ( operator_P
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

operator_P :: Parser String
operator_P = choice
  [
    -- TODO
    bang_T  *> return "!"
  , plus_T  *> return "+"
  , minus_T *> return "-"
  ]
