{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Type
  ( ConstructorOpts(..)
  , MkOpts(..)
  , constructor
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
  , docs   :: ()
  , cstrs  :: [(CoreType, CoreType, Location)]
  , kind   :: TypeKind
  , loc    :: Location
  , params :: [(CoreType, Variance)]
  , priv   :: PrivateFlag
  , text   :: ()
  }

instance Default MkOpts where
  def = MkOpts
    { attrs  = []
    , cstrs  = []
    , docs   = () -- FIXME
    , kind   = PtypeAbstract
    , loc    = defaultLoc
    , params = []
    , priv   = Public
    , text   = () -- FIXME
    }
