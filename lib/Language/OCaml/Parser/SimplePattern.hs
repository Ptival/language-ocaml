module Language.OCaml.Parser.SimplePattern
  ( simplePatternP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.SimplePatternNotIdent
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Types

simplePatternP :: Parser Pattern -> Parser Pattern
simplePatternP patternP = choice
  [ do
    i <- valIdentP
    return $ mkpat $ PpatVar $ mkRHS i 1
  , simplePatternNotIdentP'
  ]
  where
    simplePatternNotIdentP' = simplePatternNotIdentP patternP
