module Language.OCaml.Parser.SimplePatternNotIdent
  ( simplePatternNotIdentP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.SimpleDelimitedPattern
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

simplePatternNotIdentP :: Parser Pattern -> Parser Pattern
simplePatternNotIdentP patternP = choice
  [ do
    underscoreT
    return $ mkpat $ PpatAny
  -- TODO: signed constants
  , do
    i <- constrLongidentP
    return $ mkpat $ PpatConstruct (mkRHS i 1) Nothing
  -- TODO: name_tag
  -- TODO: HASH
  , simpleDelimitedPatternP'
  , do
    lParenT
    p <- patternP
    rParenT
    return $ relocPat p
  ]
  where
    simpleDelimitedPatternP' = simpleDelimitedPatternP patternP
