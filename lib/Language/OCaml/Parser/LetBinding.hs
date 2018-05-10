{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.LetBinding
  ( let_binding_P
  ) where

import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LetBindingBody
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.RecFlag
import Language.OCaml.Parser.Tokens

let_binding_P :: Parser Structure -> Parser Let_bindings
let_binding_P structure_P = do
  let_T
  -- TODO: ext_attributes
  r <- rec_flag_P
  b <- let_binding_body_P
  a <- post_item_attributes_P structure_P
  return $ mklbs Nothing r (mklb True b []) -- FIXME
