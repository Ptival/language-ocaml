module Language.OCaml.Parser.LblPatternList
  ( lblPatternListP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LblPattern
import Language.OCaml.Parser.OptSemi
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

-- FIXME: Right now, this is made to look like the OCaml parser, but we could
-- optimize everything by factoring out the left parsers that are the same.

lblPatternListP :: Parser Pattern -> Parser ([(Loc Longident, Pattern)], ClosedFlag)
lblPatternListP patternP = choice
  [ try $ do
    p <- lblPatternP'
    semiT
    (fields, closed) <- lblPatternListP'
    return (p : fields, closed)
  , try $ do
    p <- lblPatternP'
    semiT
    underscoreT
    optSemiP
    return ([p], Open)
  , try $ do
    p <- lblPatternP'
    semiT
    return ([p], Closed)
  , do
    p <- lblPatternP'
    return ([p], Closed)
  ]
  where
    lblPatternP'      = lblPatternP      patternP
    lblPatternListP' = lblPatternListP patternP
