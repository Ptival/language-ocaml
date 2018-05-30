{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.CoreType
  ( core_type_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeNoAttr
import Language.OCaml.Parser.Utils.Types

core_type_P :: Parser Core_type
core_type_P = choice
  [ core_type_no_attr_P
  -- , do
  --   t <- core_type
  --   a <- attribute
  --   return $ attr t a
  ]
