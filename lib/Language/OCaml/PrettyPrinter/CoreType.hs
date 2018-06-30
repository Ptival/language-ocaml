{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.CoreType
  ( coreTypePP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreTypeDesc ()

coreTypePP :: CoreType -> Doc a
coreTypePP = pretty . ptypDesc

instance Pretty CoreType where
  pretty = coreTypePP
