module Language.OCaml.Parser.GeneralizedConstructorArguments
  ( generalizedConstructorArgumentsP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ConstructorArguments
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

generalizedConstructorArgumentsP :: Parser CoreType -> Parser (ConstructorArguments, Maybe a)
generalizedConstructorArgumentsP coreTypeP = choice
  [ flip (,) Nothing <$> (ofT *> constructorArgumentsP coreTypeP)
    -- TODO: colon
  , return (PcstrTuple [], Nothing)
  ]
