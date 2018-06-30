module Language.OCaml.Parser.PolyTypeNoAttr
  ( polyTypeNoAttrP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.CoreTypeNoAttr
import Language.OCaml.Parser.Utils.Types

polyTypeNoAttrP :: Parser CoreType
polyTypeNoAttrP = choice
  [ coreTypenoAttrP coreTypeP
  -- TODO
  ]
