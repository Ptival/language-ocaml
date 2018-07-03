{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Str
  ( AttributeOpts(..)
  , MkOpts(..)
  , attribute
  , mk
  , text
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

attribute :: AttributeOpts -> Attribute -> StructureItem
attribute (AttributeOpts {..}) a = mk (def { loc }) (PstrAttribute a)

data AttributeOpts = AttributeOpts
  { loc :: Location
  }

instance Default AttributeOpts where
  def = AttributeOpts
    { loc = defaultLoc
    }

mk :: MkOpts -> StructureItemDesc -> StructureItem
mk (MkOpts {..}) d =
  StructureItem
  { pstrDesc = d
  , pstrLoc  = loc
  }

data MkOpts = MkOpts
  { loc :: Location
  }

instance Default MkOpts where
  def = MkOpts
    { loc = defaultLoc
    }

text :: [Docstring] -> [StructureItem]
text txt =
  let fTxt = filter (\ ds -> docstringBody ds /= "") txt in
  map (\ ds -> attribute (def { loc = docstringLoc ds }) (textAttr ds)) fTxt
