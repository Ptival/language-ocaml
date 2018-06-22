module Language.OCaml.Parser.PatternSemiList
  ( pattern_semi_list_P
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

pattern_semi_list_P :: Parser Pattern -> Parser [Pattern]
pattern_semi_list_P pattern_P = leftRecursive
  [ (: []) <$> pattern_P
  ]
  [ do
    semi_T
    e2 <- pattern_P
    return $ \ e1 -> e2 : e1
  ]
