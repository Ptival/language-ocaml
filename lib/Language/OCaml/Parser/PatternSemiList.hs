module Language.OCaml.Parser.PatternSemiList
  ( patternSemiListP
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

patternSemiListP :: Parser Pattern -> Parser [Pattern]
patternSemiListP patternP = leftRecursive
  [ (: []) <$> patternP
  ]
  [ do
    semiT
    e2 <- patternP
    return $ \ e1 -> e2 : e1
  ]
