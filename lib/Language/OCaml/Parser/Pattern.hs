module Language.OCaml.Parser.Pattern
  ( pattern_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.PatternGen
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

pattern_P :: Parser Pattern
pattern_P = choice
  [ try $ parser <* notFollowedBy comma_T
  , mkpat . Ppat_tuple <$>
    chainl1' parser (comma_T *> (return $ flip (:))) (: [])
  ]
  where
    parser = leftRecursive
      [ pattern_gen_P pattern_P
      ]
      -- pattern AS val_ident
      [ do
        try $ as_T
        i <- val_ident_P
        return $ \ x -> mkpat $ Ppat_alias x (mkRHS i 3)
      -- pattern COLONCOLON pattern
      , do
        try $ colon_colon_T
        p2 <- pattern_P
        return $ \ p1 -> mkpat_cons (rhsLoc 2) (ghpat $ Ppat_tuple [p1, p2]) (symbol_rloc ())
      , do
        try $ bar_T
        p <- pattern_P
        return $ \ x -> mkpat $ Ppat_or x p
      -- TODO: bunch of other patterns
      ]
