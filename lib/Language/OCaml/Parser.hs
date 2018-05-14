module Language.OCaml.Parser
  ( implementation_P
  , parseImplementation
  ) where

import Text.Megaparsec (parse)
import Text.Megaparsec.Error

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Implementation

parseImplementation :: String -> Either (ParseError Char Dec) Structure
parseImplementation = parse implementation_P "Language.OCaml.Parser"
