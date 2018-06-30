module Language.OCaml.Parser.MatchCase
  ( matchCaseP
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

matchCaseP :: Parser Expression -> Parser Case
matchCaseP seqExprP = choice
  [ do
    p <- try $ do
      p <- patternP
      minusGreaterT
      return p
    e <- seqExprP
    return $ caseExp def p e
  , do
    p <- try $ do
      p <- patternP
      whenT
      return p
    w <- seqExprP
    minusGreaterT
    e <- seqExprP
    return $ caseExp (def { guard = Just w }) p e
  -- TODO: others
  ]
