{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.SimpleCoreType
  ( simple_core_type_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreType2
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.Parser.Utils.Utils

simple_core_type_P :: Parser Core_type -> Parser Core_type
simple_core_type_P core_type_P = choice
  [ simple_core_type2_P
  , do
    l <- parens (sepBy core_type_P comma_T)
    case l of
      [sty] -> return sty
      _ -> fail "Parse error"
  ]
