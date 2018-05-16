module Language.OCaml.PrettyPrinter
  ( pretty
  ) where

import Data.Text.Prettyprint.Doc (pretty)

import Language.OCaml.PrettyPrinter.Payload ()
import Language.OCaml.PrettyPrinter.StructureItem ()
