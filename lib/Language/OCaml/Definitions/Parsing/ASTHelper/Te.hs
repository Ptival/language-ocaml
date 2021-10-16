{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Te
  ( DeclOpts (..),
    MkOpts (..),
    RebindOpts (..),
    decl,
    mk,
    rebind,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes
  ( Loc,
    Variance,
  )
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
    ConstructorArguments (PcstrTuple),
    CoreType,
    ExtensionConstructor (..),
    ExtensionConstructorKind (PextDecl, PextRebind),
    Longident,
    PrivateFlag (Public),
    TypeExtension (..),
  )

decl :: DeclOpts -> Maybe CoreType -> Loc String -> ExtensionConstructor
decl (DeclOpts {..}) res name =
  ExtensionConstructor
    { pextName = name,
      pextKind = PextDecl args res,
      pextLoc = loc,
      pextAttributes = addDocsAttrs docs (addInfoAttrs info attrs)
    }

data DeclOpts = DeclOpts
  { args :: ConstructorArguments,
    attrs :: [Attribute],
    docs :: Docs,
    loc :: Location,
    info :: Info
  }

instance Default DeclOpts where
  def =
    DeclOpts
      { args = PcstrTuple [],
        attrs = [],
        docs = emptyDocs,
        loc = defaultLoc,
        info = emptyInfo
      }

mk :: MkOpts -> Loc Longident -> [ExtensionConstructor] -> TypeExtension
mk (MkOpts {..}) path constructors =
  TypeExtension
    { ptyextPath = path,
      ptyextParams = params,
      ptyextConstructors = constructors,
      ptyextPrivate = priv,
      ptyextAttributes = attrs
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    docs :: Docs,
    params :: [(CoreType, Variance)],
    priv :: PrivateFlag
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        docs = emptyDocs,
        params = [],
        priv = Public
      }

rebind :: RebindOpts -> Loc String -> Loc Longident -> ExtensionConstructor
rebind (RebindOpts {..}) name lid =
  ExtensionConstructor
    { pextName = name,
      pextKind = PextRebind lid,
      pextLoc = loc,
      pextAttributes = addDocsAttrs docs $ addInfoAttrs info $ attrs
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
