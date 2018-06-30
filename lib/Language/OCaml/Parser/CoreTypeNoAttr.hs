module Language.OCaml.Parser.CoreTypeNoAttr
  ( coreTypenoAttrP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType2
import Language.OCaml.Parser.Utils.Types

coreTypenoAttrP :: Parser CoreType -> Parser CoreType
coreTypenoAttrP coreTypeP = choice
  [ coreType2P coreTypeP
  -- , TODO
  ]
