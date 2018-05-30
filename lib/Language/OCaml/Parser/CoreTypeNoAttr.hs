module Language.OCaml.Parser.CoreTypeNoAttr
  ( core_type_no_attr_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType2
import Language.OCaml.Parser.Utils.Types

core_type_no_attr_P :: Parser Core_type
core_type_no_attr_P = choice
  [ core_type2_P
  -- , TODO
  ]
