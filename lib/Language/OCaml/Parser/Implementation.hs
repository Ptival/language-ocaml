module Language.OCaml.Parser.Implementation
  ( implementationP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Structure
import Language.OCaml.Parser.Utils.Utils
import Language.OCaml.Parser.Utils.Types

implementationP :: Parser Structure
implementationP = ocamlSpace *> structureP <* eof
