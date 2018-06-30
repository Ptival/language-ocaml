module Language.OCaml.Parser.FunBinding
  ( funBindingP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.TypeConstraint
import Language.OCaml.Parser.StrictBinding
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

funBindingP :: Parser Expression -> Parser Expression
funBindingP seqExprP = choice
  [ strictBindingP seqExprP (funBindingP seqExprP)
  , do
    c <- typeConstraintP
    equalT
    e <- seqExprP
    return $ mkexpConstraint e c
  ]
