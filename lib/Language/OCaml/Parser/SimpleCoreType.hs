module Language.OCaml.Parser.SimpleCoreType
  ( simpleCoreTypeP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeCommaList
import Language.OCaml.Parser.SimpleCoreType2
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.Parser.Utils.Utils

simpleCoreTypeP :: Parser CoreType -> Parser CoreType
simpleCoreTypeP coreTypeP = choice
  [ try $ simpleCoreType2P coreTypeP
  , do
    l <- parens $ coreTypeCommaListP coreTypeP
    case l of
      [sty] -> return sty
      _ -> fail "Parse error"
  ]
