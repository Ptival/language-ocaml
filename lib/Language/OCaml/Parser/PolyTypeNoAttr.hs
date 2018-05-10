{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.PolyTypeNoAttr
  ( poly_type_no_attr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeNoAttr

poly_type_no_attr_P :: Parser Core_type
poly_type_no_attr_P = choice
  [ core_type_no_attr_P
  -- TODO
  ]
