{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.SimpleCoreType
  ( simple_core_type_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreType2
import Language.OCaml.Parser.Utils.Types

simple_core_type_P :: Parser Core_type
simple_core_type_P = choice
  [ simple_core_type2_P
  -- , parens core_type_comma_list
  ]
