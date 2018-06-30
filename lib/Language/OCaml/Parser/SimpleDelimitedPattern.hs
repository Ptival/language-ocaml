module Language.OCaml.Parser.SimpleDelimitedPattern
  ( simpleDelimitedPatternP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LblPatternList
import Language.OCaml.Parser.OptSemi
import Language.OCaml.Parser.PatternSemiList
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

simpleDelimitedPatternP :: Parser Pattern -> Parser Pattern
simpleDelimitedPatternP patternP = choice
  [ do
    (fields, closed) <- try $ do
      lBraceT
      lblPatternListP patternP
    rBraceT
    return $ mkpat $ PpatRecord fields closed
  , do
    l <- try $ do
      lBracketT
      l <- patternSemiListP patternP
      optSemiP
      return l
    rBracketT
    return $ mktailpat (rhsLoc 4) (reverse l)
  , do
    l <- try $ do
      lBracketBarT
      l <- patternSemiListP'
      optSemiP
      return l
    barRBracketT
    return $ mkpat $ PpatArray (reverse l)
  , do
    lBracketBarT
    barRBracketT
    return $ mkpat $ PpatArray []
  ]
  where
    patternSemiListP' = patternSemiListP patternP
