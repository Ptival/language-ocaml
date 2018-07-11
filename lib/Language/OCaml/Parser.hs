module Language.OCaml.Parser
  ( ParserC
  , ParserG
  , implementationP
  , parseImplementationC
  , parseImplementationG
  ) where

import           Data.Void
import           Text.Megaparsec (parse)
import           Text.Megaparsec.Error

import           Language.OCaml.Definitions.Parsing.ParseTree
-- import qualified Language.OCaml.Parser.Generator.Lexer as GL
import qualified Language.OCaml.Parser.Generator.Parser as GP
import           Language.OCaml.Parser.Implementation
import           Language.OCaml.Parser.Utils.Types

type ParserC a = Parser a

type ParserG a = GP.Parser a

parseImplementationC :: String -> Either (ParseError Char Void) Structure
parseImplementationC = parse implementationP "Language.OCaml.Parser"

parseImplementationG :: GP.Parser [StructureItem]
parseImplementationG = GP.parseImplementation
