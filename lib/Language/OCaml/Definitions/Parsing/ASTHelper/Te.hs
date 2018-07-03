{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Te
  ( DeclOpts(..)
  , MkOpts(..)
  , decl
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTTypes hiding (loc)
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.ParseTree

decl :: DeclOpts -> Maybe CoreType -> Loc String -> ExtensionConstructor
decl (DeclOpts {..}) res name = ExtensionConstructor
  { pextName       = name
  , pextKind       = PextDecl args res
  , pextLoc        = loc
  , pextAttributes = addDocsAttrs docs (addInfoAttrs info attrs)
  }

data DeclOpts = DeclOpts
  { args   :: ConstructorArguments
  , attrs  :: [Attribute]
  , docs   :: Docs
  , loc    :: Location
  , info   :: Info
  }

instance Default DeclOpts where
  def = DeclOpts
    { args   = PcstrTuple []
    , attrs  = []
    , docs   = emptyDocs
    , loc    = defaultLoc
    , info   = emptyInfo
    }

mk :: MkOpts -> Loc Longident -> [ExtensionConstructor] -> TypeExtension
mk (MkOpts {..}) path constructors = TypeExtension
  { ptyextPath         = path
  , ptyextParams       = params
  , ptyextConstructors = constructors
  , ptyextPrivate      = priv
  , ptyextAttributes   = attrs
  }

data MkOpts = MkOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  , params :: [(CoreType, Variance)]
  , priv   :: PrivateFlag
  }

instance Default MkOpts where
  def = MkOpts
    { attrs  = []
    , docs   = emptyDocs
    , params = []
    , priv   = Public
    }
