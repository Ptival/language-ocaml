module Language.OCaml.Parser.Payload
  ( payload_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Utils.Types

payload_P :: Parser Structure -> Parser Payload
payload_P structure_P = choice
  [ PStr <$> structure_P
  ]
