{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.CoreType
  ( coreTypePP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( CoreType (ptypDesc),
  )
import Language.OCaml.PrettyPrinter.CoreTypeDesc ()
import Prettyprinter (Doc, Pretty (pretty))

coreTypePP :: CoreType -> Doc a
coreTypePP = pretty . ptypDesc

instance Pretty CoreType where
  pretty = coreTypePP
