module Language.OCaml.Parser.Payload
  ( payload_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree

payload_P :: Parser Structure -> Parser Payload
payload_P structure_P = choice
  [ PStr <$> structure_P
  ]
