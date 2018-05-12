module Language.OCaml.Parser.LetBindings
  ( let_bindings_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Parser.LetBindings
import Language.OCaml.Parser.Common
-- import Language.OCaml.Parser.Attributes
import Language.OCaml.Parser.LetBinding
import Language.OCaml.Parser.LetBindingBody
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators

let_bindings_P :: Parser Structure -> Parser Expression -> Parser Let_bindings
let_bindings_P structure_P seq_expr_P = leftRecursive
  [ let_binding_P structure_P seq_expr_P
  ]
  [ do
    try $ and_T
    -- a <- attributes_P
    b <- let_binding_body_P seq_expr_P
    p <- post_item_attributes_P structure_P
    return $ \ x -> addlb x (mklb False b []) -- FIXME
  ]
