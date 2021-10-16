{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.ArgLabel
  ( argLabelPP,
  )
where

import Language.OCaml.Definitions.Parsing.ASTTypes
  ( ArgLabel (Nolabel),
  )
import Language.OCaml.PrettyPrinter.Case ()
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Prettyprinter (Doc, Pretty (pretty))

argLabelPP :: ArgLabel -> Doc a
argLabelPP = \case
  Nolabel -> ""
  _ -> error "TODO"

instance Pretty ArgLabel where
  pretty = argLabelPP
