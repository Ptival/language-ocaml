module Language.OCaml.Parser.Operator
  ( operatorP
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

operatorP :: Parser String
operatorP = choice
  [
    -- TODO
    bangT  *> return "!"
  , plusT  *> return "+"
  , minusT *> return "-"
  ]
