{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Type
  ( ConstructorOpts(..)
  , FieldOpts(..)
  , MkOpts(..)
  , constructor
  , field
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

constructor ::
  ConstructorOpts ->
  Maybe CoreType ->
  Loc String ->
  ConstructorDeclaration
constructor (ConstructorOpts {..}) res name =
  ConstructorDeclaration
  { pcdName       = name
  , pcdArgs       = args
  , pcdRes        = res
  , pcdLoc        = loc
  , pcdAttributes = attrs
  }

data ConstructorOpts = ConstructorOpts
  { args   :: ConstructorArguments
  , attrs  :: [Attribute]
  , loc    :: Location
  , info   :: Info
  }

instance Default ConstructorOpts where
  def = ConstructorOpts
    { args = PcstrTuple []
    , attrs = []
    , loc   = defaultLoc
    , info  = emptyInfo
    }

field :: FieldOpts -> Loc String -> CoreType -> LabelDeclaration
field (FieldOpts {..}) name typ =
  LabelDeclaration
  { pldName       = name
  , pldMutable    = mut
  , pldType       = typ
  , pldLoc        = loc
  , pldAttributes = addInfoAttrs info attrs
  }

data FieldOpts = FieldOpts
  { attrs  :: [Attribute]
  , info   :: Info
  , loc    :: Location
  , mut    :: MutableFlag
  }

instance Default FieldOpts where
  def = FieldOpts
    { attrs  = []
    , info   = emptyInfo
    , loc    = defaultLoc
    , mut    = Immutable
    }

mk :: MkOpts -> Maybe CoreType -> Loc String -> TypeDeclaration
mk (MkOpts {..}) manifest name =
  TypeDeclaration
  { ptypeName       = name
  , ptypeParams     = params
  , ptypeCstrs      = cstrs
  , ptypeKind       = kind
  , ptypePrivate    = priv
  , ptypeManifest   = manifest
  , ptypeAttributes = attrs
  , ptypeLoc        = loc
  }

data MkOpts = MkOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  , cstrs  :: [(CoreType, CoreType, Location)]
  , kind   :: TypeKind
  , loc    :: Location
  , params :: [(CoreType, Variance)]
  , priv   :: PrivateFlag
  , text   :: Text
  }

instance Default MkOpts where
  def = MkOpts
    { attrs  = []
    , cstrs  = []
    , docs   = emptyDocs
    , kind   = PtypeAbstract
    , loc    = defaultLoc
    , params = []
    , priv   = Public
    , text   = []
    }
