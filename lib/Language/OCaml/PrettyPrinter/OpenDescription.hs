{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.OpenDescription
  ( openDescriptionPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Language.OCaml.PrettyPrinter.OverrideFlag ()

openDescriptionPP :: OpenDescription -> Doc a
openDescriptionPP d = fillCat
  [ "open"
  , space
  , pretty $ popenOverride d
  , pretty $ popenLid d
  ]

instance Pretty OpenDescription where
  pretty = openDescriptionPP
