module Language.OCaml.Parser.MatchCases
  ( match_cases_P
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.MatchCase
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

match_cases_P :: Parser Expression -> Parser [Case]
match_cases_P seq_expr_P =
  chainl1' (match_case_P seq_expr_P) (bar_T *> return (flip (:))) (: [])
