module Language.OCaml.Parser.MatchCases
  ( matchCasesP
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.MatchCase
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

matchCasesP :: Parser Expression -> Parser [Case]
matchCasesP seqExprP =
  chainl1' (matchCaseP seqExprP) (barT *> return (flip (:))) (: [])
