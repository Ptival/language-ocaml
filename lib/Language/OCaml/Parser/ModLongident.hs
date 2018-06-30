module Language.OCaml.Parser.ModLongident
  ( modLongidentP
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

modLongidentP :: Parser Longident
modLongidentP = chainl1try' uIdentT (dotT *> return Ldot) Lident
