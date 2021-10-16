{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Opn
  ( MkOpts (..),
    mk,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes
  ( Loc,
    OverrideFlag (Fresh),
  )
import Language.OCaml.Definitions.Parsing.Docstrings
  ( Docs,
    addDocsAttrs,
    emptyDocs,
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    Longident,
    OpenDescription (..),
  )

mk :: MkOpts -> Loc Longident -> OpenDescription
mk (MkOpts {..}) lid =
  OpenDescription
    { popenLid = lid,
      popenOverride = override,
      popenLoc = loc,
      popenAttributes = addDocsAttrs docs attrs
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    docs :: Docs,
    loc :: Location,
    override :: OverrideFlag
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        docs = emptyDocs,
        loc = defaultLoc,
        override = Fresh
      }
