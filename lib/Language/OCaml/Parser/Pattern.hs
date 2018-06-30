module Language.OCaml.Parser.Pattern
  ( patternP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.PatternGen
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

patternP :: Parser Pattern
patternP = choice
  [ try $ parser <* notFollowedBy commaT
  , mkpat . PpatTuple <$>
    chainl1' parser (commaT *> (return $ flip (:))) (: [])
  ]
  where
    parser = leftRecursive
      [ patternGenP patternP
      ]
      -- pattern AS valIdent
      [ do
        try $ asT
        i <- valIdentP
        return $ \ x -> mkpat $ PpatAlias x (mkRHS i 3)
      -- pattern COLONCOLON pattern
      , do
        try $ colonColonT
        p2 <- patternP
        return $ \ p1 -> mkpatCons (rhsLoc 2) (ghpat $ PpatTuple [p1, p2]) (symbolRLoc ())
      , do
        try $ barT
        p <- patternP
        return $ \ x -> mkpat $ PpatOr x p
      -- TODO: bunch of other patterns
      ]
