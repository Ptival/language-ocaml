{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mty
  ( MkOpts(..)
  , attr
  , extension
  , ident
  , mk
  ) where

import Data.Default
import Prelude                                             hiding (sequence)

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree        as ParseTree
import Language.OCaml.Definitions.Parsing.Location

attr :: ModuleType -> Attribute -> ModuleType
attr d a = d { pmtyAttributes = pmtyAttributes d ++ [a] }

mk :: MkOpts -> ModuleTypeDesc -> ModuleType
mk (MkOpts {..}) desc =
  ModuleType
  { pmtyDesc       = desc
  , pmtyLoc        = loc
  , pmtyAttributes = attrs
  }

data MkOpts = MkOpts
  { attrs  :: [Attribute]
  , loc    :: Location
  }

instance Default MkOpts where
  def = MkOpts
    { attrs  = []
    , loc    = defaultLoc
    }

ident :: MkOpts -> Loc Longident -> ModuleType
ident (MkOpts {..}) a = mk (def { loc, attrs }) $ PmtyIdent a

extension :: MkOpts -> Extension -> ModuleType
extension (MkOpts {..}) a = mk (def { loc, attrs }) $ PmtyExtension a
