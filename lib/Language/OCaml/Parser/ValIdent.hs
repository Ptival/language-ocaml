module Language.OCaml.Parser.ValIdent
  ( valIdentP
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Operator
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

valIdentP :: Parser String
valIdentP = choice
  [ lIdentT
  , do
    o <- try $ do
      lParenT
      operatorP
    rParenT
    return o
  ]
