{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.CoreType2
  ( core_type2_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreTypeOrTuple
import Language.OCaml.Parser.Utils.Types

core_type2_P :: Parser Core_type
core_type2_P = choice
  [ simple_core_type_or_tuple_P
  -- TODO
  ]
