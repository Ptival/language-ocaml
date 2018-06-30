module Language.OCaml.Parser.TypeVariable
  ( typeVariableP
  ) where


import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

typeVariableP :: Parser CoreType
typeVariableP = mktyp . PtypVar <$> (quoteT *> identP)
