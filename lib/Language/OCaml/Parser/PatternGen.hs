module Language.OCaml.Parser.PatternGen
  ( patternGenP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.SimplePattern
import Language.OCaml.Parser.Utils.Types

patternGenP :: Parser Pattern -> Parser Pattern
patternGenP patternP = choice
  [ try $ do
    i <- constrLongidentP
    p <- patternP
    return $ mkpat $ PpatConstruct (mkRHS i 1) (Just p)
  , try $ simplePatternP'
  ]
  where
    simplePatternP' = simplePatternP patternP
