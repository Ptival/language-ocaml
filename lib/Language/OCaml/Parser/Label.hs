module Language.OCaml.Parser.Label
  ( labelP
  ) where

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

labelP :: Parser String
labelP = lIdentT
