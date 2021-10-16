{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Str
  ( AttributeOpts (..),
    MkOpts (..),
    attribute,
    mk,
    text,
  )
where

import Data.Default ( Default(..) )
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
    ( defaultLoc )
import Language.OCaml.Definitions.Parsing.Docstrings
    ( Docstring, textAttr, docstringBody, docstringLoc )
import Language.OCaml.Definitions.Parsing.Location ( Location )
import Language.OCaml.Definitions.Parsing.ParseTree
    ( StructureItemDesc(PstrAttribute), StructureItem(..), Attribute )

attribute :: AttributeOpts -> Attribute -> StructureItem
attribute (AttributeOpts {..}) a = mk (def {loc}) (PstrAttribute a)

newtype AttributeOpts = AttributeOpts
  { loc :: Location
  }

instance Default AttributeOpts where
  def =
    AttributeOpts
      { loc = defaultLoc
      }

mk :: MkOpts -> StructureItemDesc -> StructureItem
mk (MkOpts {..}) d =
  StructureItem
    { pstrDesc = d,
      pstrLoc = loc
    }

newtype MkOpts = MkOpts
  { loc :: Location
  }

instance Default MkOpts where
  def =
    MkOpts
      { loc = defaultLoc
      }

text :: [Docstring] -> [StructureItem]
text txt =
  let fTxt = filter (\ds -> docstringBody ds /= "") txt
   in map (\ds -> attribute (def {loc = docstringLoc ds}) (textAttr ds)) fTxt
