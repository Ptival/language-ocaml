module Language.OCaml.Parser.OptionalTypeVariable
  ( optionalTypeVariableP
  ) where


import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

optionalTypeVariableP :: Parser CoreType
optionalTypeVariableP = mktyp . PtypVar <$> (quoteT *> identP)
