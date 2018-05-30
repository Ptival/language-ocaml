{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.Structure
  ( structure_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.SeqExpr
import Language.OCaml.Parser.StructureItem
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Utils
import Language.OCaml.Parser.Utils.Types

structure_P :: Parser Structure
structure_P = ocamlSpace *> choice
  [ do
    e <- seq_expr_P structure_P
    a <- post_item_attributes_P structure_P
    s <- structure_tail_P
    return $ text_str 1 ++ mkstrexp e a : s
  , structure_tail_P
  ]
  where
    structure_tail_P :: Parser [Structure_item]
    structure_tail_P = choice
      [ do
        (i, t) <- try $ do
          i <- structure_item_P structure_P
          t <- structure_tail_P
          return (i, t)
        return $ text_str 1 ++ i : t
      , do
        try $ semi_semi_T
        s <- structure_P
        return $ text_str 1 ++ s
      , ocamlSpace *> return []
      ]
