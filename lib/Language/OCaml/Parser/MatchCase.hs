module Language.OCaml.Parser.MatchCase
  ( match_case_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

match_case_P :: Parser Expression -> Parser Case
match_case_P seq_expr_P = choice
  [ do
    p <- try $ do
      p <- pattern_P
      minus_greater_T
      return p
    e <- seq_expr_P
    return $ caseExp p Nothing e
  , do
    p <- try $ do
      p <- pattern_P
      when_T
      return p
    w <- seq_expr_P
    minus_greater_T
    e <- seq_expr_P
    return $ caseExp p (Just w) e
  -- TODO: others
  ]
