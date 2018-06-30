module Language.OCaml.Parser.Payload
  ( payloadP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Utils.Types

payloadP :: Parser Structure -> Parser Payload
payloadP structureP = choice
  [ PStr <$> structureP
  ]
