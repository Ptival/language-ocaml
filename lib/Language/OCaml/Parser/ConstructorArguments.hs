module Language.OCaml.Parser.ConstructorArguments
  ( constructorArgumentsP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreTypeList
import Language.OCaml.Parser.Utils.Types

constructorArgumentsP :: Parser CoreType -> Parser ConstructorArguments
constructorArgumentsP coreTypeP = choice
  [ PcstrTuple . reverse <$> coreTypelistP coreTypeP
  -- TODO: label declarations
  ]
