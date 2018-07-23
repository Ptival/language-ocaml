{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mtd
  ( MkOpts(..)
  , mk
  ) where

import Data.Default
import Prelude hiding (sequence)

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.ParseTree as ParseTree
import Language.OCaml.Definitions.Parsing.Location

mk :: MkOpts -> Maybe ModuleType -> Loc String -> ModuleTypeDeclaration
mk (MkOpts {..}) typ name =
  ModuleTypeDeclaration
  { pmtdName       = name
  , pmtdType       = typ
  , pmtdAttributes = addTextAttrs text $ addDocsAttrs docs $ attrs
  , pmtdLoc        = loc
  }

data MkOpts = MkOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  , loc    :: Location
  , text   :: Text
  }

instance Default MkOpts where
  def = MkOpts
    { attrs = []
    , docs  = emptyDocs
    , loc   = defaultLoc
    , text  = []
    }
