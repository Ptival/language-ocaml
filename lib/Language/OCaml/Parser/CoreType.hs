module Language.OCaml.Parser.CoreType
  ( coreTypeP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeNoAttr
import Language.OCaml.Parser.Utils.Types

coreTypeP :: Parser CoreType
coreTypeP = choice
  [ coreTypenoAttrP coreTypeP
  -- , do
  --   t <- coreType
  --   a <- attribute
  --   return $ attr t a
  ]
