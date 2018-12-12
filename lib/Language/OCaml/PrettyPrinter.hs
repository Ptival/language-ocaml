module Language.OCaml.PrettyPrinter
  ( pretty
  , payloadPP
  , structurePP
  , structureItemPP
  ) where

import           Data.Text.Prettyprint.Doc                    (Doc, pretty)

import           Language.OCaml.Definitions.Parsing.ParseTree
import qualified Language.OCaml.PrettyPrinter.Payload         as Payload
import qualified Language.OCaml.PrettyPrinter.Structure       as Structure
import qualified Language.OCaml.PrettyPrinter.StructureItem   as StructureItem

-- tying some knots

payloadPP :: Payload -> Doc a
payloadPP = Payload.payloadPP

structureItemPP :: StructureItem -> Doc a
structureItemPP = StructureItem.structureItemPP

structurePP :: Structure -> Doc a
structurePP = Structure.structurePP
