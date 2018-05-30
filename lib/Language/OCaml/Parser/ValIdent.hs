{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.ValIdent
  ( val_ident_P
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Operator
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

val_ident_P :: Parser String
val_ident_P = choice
  [ l_ident_T
  , do
    o <- try $ do
      l_paren_T
      operator_P
    r_paren_T
    return o
  ]
