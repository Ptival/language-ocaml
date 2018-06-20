module Language.OCaml.Parser.ExprSemiList
  ( expr_semi_list_P
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

expr_semi_list_P :: Parser Expression -> Parser [Expression]
expr_semi_list_P expr_P = leftRecursive
  [ (: []) <$> expr_P
  ]
  [ do
    semi_T
    e2 <- expr_P
    return $ \ e1 -> e2 : e1
  ]
