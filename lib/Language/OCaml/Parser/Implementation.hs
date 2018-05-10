module Language.OCaml.Parser.Implementation
  ( implementation_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Structure
import Language.OCaml.Parser.Utils.Utils

implementation_P :: Parser Structure
implementation_P = ocamlSpace *> structure_P <* eof
