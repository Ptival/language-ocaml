{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.CoreTypeDesc
  ( core_type_desc_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()

core_type_desc_PP :: (Pretty Core_type) => Core_type_desc -> Doc a
core_type_desc_PP = \case
  Ptyp_any -> "_"
  Ptyp_var s -> fillCat [ squote, pretty s ]
  Ptyp_tuple l -> fillSep $ map pretty l
  Ptyp_constr i l -> fillSep $ pretty i : map pretty l

instance Pretty Core_type => Pretty Core_type_desc where
  pretty = core_type_desc_PP
