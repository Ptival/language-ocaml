{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Loc
  ( locPP,
  )
where

import Language.OCaml.Definitions.Parsing.ASTTypes (Loc (txt))
import Prettyprinter (Doc, Pretty (pretty))

locPP :: (Pretty a) => Loc a -> Doc b
locPP = pretty . txt

instance Pretty a => Pretty (Loc a) where
  pretty = locPP
