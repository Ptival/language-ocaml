{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Loc
  ( loc_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes

loc_PP :: (Pretty a) => Loc a -> Doc b
loc_PP = pretty . txt

instance Pretty a => Pretty (Loc a) where
  pretty = loc_PP
