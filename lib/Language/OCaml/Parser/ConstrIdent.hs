module Language.OCaml.Parser.ConstrIdent
  ( constrIdentP
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

constrIdentP :: Parser String
constrIdentP = choice
  [ uIdentT
  , lBracketT *> rBracketT *> return "[]"
  , lParenT *> rParenT *> return "()"
  , lParenT *> colonColonT *> rParenT *> return "::"
  , falseT *> return "false"
  , trueT *> return "true"
  ]
