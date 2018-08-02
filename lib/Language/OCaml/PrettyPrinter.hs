module Language.OCaml.PrettyPrinter
  ( pretty
  , structurePP
  ) where

import Data.Text.Prettyprint.Doc                  (pretty)

import Language.OCaml.PrettyPrinter.Payload       ()
import Language.OCaml.PrettyPrinter.Structure
import Language.OCaml.PrettyPrinter.StructureItem ()
