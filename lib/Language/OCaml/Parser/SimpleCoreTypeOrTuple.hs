module Language.OCaml.Parser.SimpleCoreTypeOrTuple
  ( simpleCoreTypeOrTupleP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.CoreTypeList
import Language.OCaml.Parser.SimpleCoreType
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

simpleCoreTypeOrTupleP :: Parser CoreType -> Parser CoreType
simpleCoreTypeOrTupleP coreTypeP = choice
  [ do
    t <- try $ do
      t <- simpleCoreTypeP'
      starT
      return t
    l <- coreTypelistP coreTypeP
    return . mktyp . PtypTuple $ t : reverse l
  , simpleCoreTypeP'
  ]
  where
    simpleCoreTypeP' = simpleCoreTypeP coreTypeP
