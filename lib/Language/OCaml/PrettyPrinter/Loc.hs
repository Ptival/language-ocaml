{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Loc
  ( locPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes

locPP :: (Pretty a) => Loc a -> Doc b
locPP = pretty . txt

instance Pretty a => Pretty (Loc a) where
  pretty = locPP
