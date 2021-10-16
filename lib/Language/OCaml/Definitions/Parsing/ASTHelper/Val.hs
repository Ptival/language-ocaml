{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Val
  ( MkOpts (..),
    mk,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes (Loc)
import Language.OCaml.Definitions.Parsing.Docstrings
  ( Docs,
    addDocsAttrs,
    emptyDocs,
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    CoreType,
    ValueDescription (..),
  )

mk :: MkOpts -> Loc String -> CoreType -> ValueDescription
mk (MkOpts {..}) name typ =
  ValueDescription
    { pvalName = name,
      pvalType = typ,
      pvalAttributes = addDocsAttrs docs attrs,
      pvalLoc = loc,
      pvalPrim = prim
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    docs :: Docs,
    loc :: Location,
    prim :: [String]
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        docs = emptyDocs,
        loc = defaultLoc,
        prim = []
      }
