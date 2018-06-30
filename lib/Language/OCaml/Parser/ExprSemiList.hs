module Language.OCaml.Parser.ExprSemiList
  ( exprSemiListP
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

exprSemiListP :: Parser Expression -> Parser [Expression]
exprSemiListP exprP = leftRecursive
  [ (: []) <$> exprP
  ]
  [ do
    semiT
    e2 <- exprP
    return $ \ e1 -> e2 : e1
  ]
