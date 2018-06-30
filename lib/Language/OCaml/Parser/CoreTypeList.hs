module Language.OCaml.Parser.CoreTypeList
  ( coreTypelistP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreType
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

coreTypelistP :: Parser CoreType -> Parser [CoreType]
coreTypelistP coreTypeP = leftRecursive
  [ (: []) <$> simpleCoreTypeP coreTypeP
  ]
  [ do
    try $ starT
    t <- simpleCoreTypeP coreTypeP
    return $ (:) t
  ]
