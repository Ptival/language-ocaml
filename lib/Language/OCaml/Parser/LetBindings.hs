module Language.OCaml.Parser.LetBindings
  ( letBindingsP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Parser.LetBindings
import Language.OCaml.Parser.Common
-- import Language.OCaml.Parser.Attributes
import Language.OCaml.Parser.LetBinding
import Language.OCaml.Parser.LetBindingBody
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

letBindingsP :: Parser Structure -> Parser Expression -> Parser LetBindings
letBindingsP structureP seqExprP = leftRecursive
  [ letBindingP structureP seqExprP
  ]
  [ do
    try $ andT
    -- a <- attributesP
    b <- letBindingBodyP seqExprP
    _p <- postItemAttributesP structureP
    return $ \ x -> addlb x (mklb False b []) -- FIXME
  ]
