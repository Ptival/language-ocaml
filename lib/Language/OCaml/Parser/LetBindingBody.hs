{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.LetBindingBody
  ( let_binding_body_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.StrictBinding
import Language.OCaml.Parser.ValIdent

let_binding_body_P :: Parser (Pattern, Expression)
let_binding_body_P = choice
  [ do
    i <- val_ident_P
    b <- strict_binding_P
    return $ (mkpatvar i 1, b)
  ]
