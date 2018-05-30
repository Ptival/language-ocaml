{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
  [ try $ p <* notFollowedBy comma_T
  , mkpat . Ppat_tuple <$>
    chainl1' p (comma_T *> (return $ flip (:))) (: [])
  ]
  where
    p = leftRecursive
      [ pattern_gen_P pattern_P
      ]
      [ do
        try $ as_T
        i <- val_ident_P
        return $ \ x -> mkpat $ Ppat_alias x (mkRHS i 3)
      -- TODO: pattern COLONCOLON pattern
      , do
        try $ bar_T
        p <- pattern_P
        return $ \ x -> mkpat $ Ppat_or x p
      -- TODO: bunch of other patterns
      ]
