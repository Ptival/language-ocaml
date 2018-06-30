module Language.OCaml.Parser.CoreTypeCommaList
  ( coreTypeCommaListP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

coreTypeCommaListP :: Parser CoreType -> Parser [CoreType]
coreTypeCommaListP coreTypeP = sepBy coreTypeP commaT
