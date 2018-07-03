{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Typ
  ( MkOpts(..)
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.ParseTree

mk :: MkOpts -> CoreTypeDesc -> CoreType
mk (MkOpts {..}) desc =
  CoreType
  { ptypDesc       = desc
  , ptypLoc        = loc
  , ptypAttributes = attrs
  }

data MkOpts = MkOpts
  { loc   :: Location
  , attrs :: [Attribute]
  }

instance Default MkOpts where
  def = MkOpts
    { loc   = defaultLoc
    , attrs = []
    }
