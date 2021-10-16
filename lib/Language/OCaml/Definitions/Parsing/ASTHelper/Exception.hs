{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Exception
  ( MkOpts (..),
    RebindOpts (..),
    mk,
    rebind,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes (Loc)
import Language.OCaml.Definitions.Parsing.Docstrings
  ( Docs,
    Info,
    addDocsAttrs,
    addInfoAttrs,
    emptyDocs,
    emptyInfo,
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    ExtensionConstructor (..),
    ExtensionConstructorKind (PextRebind),
    Longident,
    TypeException (..),
  )

mk :: MkOpts -> ExtensionConstructor -> TypeException
mk (MkOpts {..}) ctor =
  TypeException
    { ptyexnConstructor = ctor,
      ptyexnAttributes = attrs
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    docs :: Docs
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        docs = emptyDocs
      }

rebind :: RebindOpts -> Loc String -> Loc Longident -> ExtensionConstructor
rebind (RebindOpts {..}) name lid =
  ExtensionConstructor
    { pextName = name,
      pextKind = PextRebind lid,
      pextLoc = loc,
      pextAttributes = addDocsAttrs docs (addInfoAttrs info attrs)
    }

data RebindOpts = RebindOpts
  { attrs :: [Attribute],
    docs :: Docs,
    loc :: Location,
    info :: Info
  }

instance Default RebindOpts where
  def =
    RebindOpts
      { attrs = [],
        docs = emptyDocs,
        loc = defaultLoc,
        info = emptyInfo
      }
