module Language.OCaml.Parser
  ( implementation_P
  , parseImplementationC
  , parseImplementationG
  ) where

import           Data.Void
import           Text.Megaparsec (parse)
import           Text.Megaparsec.Error

import           Language.OCaml.Definitions.Parsing.ParseTree
import qualified Language.OCaml.Parser.Generator.Parser as GP
import           Language.OCaml.Parser.Implementation

parseImplementationC :: String -> Either (ParseError Char Void) Structure
parseImplementationC = parse implementation_P "Language.OCaml.Parser"

parseImplementationG :: GP.Parser ()
parseImplementationG = GP.parseImplementation
