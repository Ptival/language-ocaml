{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.ModuleBinding
  ( module_binding_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ModuleBindingBody
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

module_binding_P :: Parser Structure -> Parser (Module_binding, ())
module_binding_P structure_P = do
  try $ module_T
  -- TODO: ext_attributes
  i <- u_ident_T
  b <- module_binding_body_P
  _a <- post_item_attributes_P structure_P
  return $ (mkMb Nothing Nothing Nothing Nothing (mkRHS i 3) b -- FIXME
           , () -- FIXME
           )
