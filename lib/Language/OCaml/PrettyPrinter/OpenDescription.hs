{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.OpenDescription
  ( open_description_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Language.OCaml.PrettyPrinter.OverrideFlag ()

open_description_PP :: Open_description -> Doc a
open_description_PP d = fillCat
  [ "open"
  , space
  , pretty $ popen_override d
  , pretty $ popen_lid d
  ]

instance Pretty Open_description where
  pretty = open_description_PP
