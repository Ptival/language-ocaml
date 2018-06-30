module Language.OCaml.Parser.FunDef
  ( funDefP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LabeledSimplePattern
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.PrettyPrinter ()

funDefP :: Parser Expression -> Parser Expression
funDefP seqExprP = choice
  [ do
    minusGreaterT
    seqExprP
  -- TODO
  , do
    (l, o, p) <- labeledSimplePatternP patternP
    d <- funDefP'
    return $ ghexp $ PexpFun l o p d
  ]
  where
    funDefP' = funDefP seqExprP
