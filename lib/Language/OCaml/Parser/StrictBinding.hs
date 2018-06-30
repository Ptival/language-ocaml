module Language.OCaml.Parser.StrictBinding
  ( strictBindingP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabeledSimplePattern
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

strictBindingP :: Parser Expression -> Parser Expression -> Parser Expression
strictBindingP seqExprP funBindingP = choice
  [ do
    try $ equalT
    seqExprP
  , do
    (l, o, p) <- labeledSimplePatternP patternP
    b <- funBindingP
    return $ ghexp $ PexpFun l o p b
    -- TODO
  ]
