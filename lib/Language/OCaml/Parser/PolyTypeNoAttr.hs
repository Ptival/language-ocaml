module Language.OCaml.Parser.PolyTypeNoAttr
  ( poly_type_no_attr_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.CoreTypeNoAttr
import Language.OCaml.Parser.Utils.Types

poly_type_no_attr_P :: Parser Core_type
poly_type_no_attr_P = choice
  [ core_type_no_attr_P core_type_P
  -- TODO
  ]
