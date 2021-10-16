{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mty
  ( MkOpts (..),
    attr,
    extension,
    ident,
    mk,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes (Loc)
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree as ParseTree
  ( Attribute,
    Extension,
    Longident,
    ModuleType (..),
    ModuleTypeDesc (PmtyExtension, PmtyIdent),
  )
import Prelude hiding (sequence)

attr :: ModuleType -> Attribute -> ModuleType
attr d a = d {pmtyAttributes = pmtyAttributes d ++ [a]}

mk :: MkOpts -> ModuleTypeDesc -> ModuleType
mk (MkOpts {..}) desc =
  ModuleType
    { pmtyDesc = desc,
      pmtyLoc = loc,
      pmtyAttributes = attrs
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    loc :: Location
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        loc = defaultLoc
      }

ident :: MkOpts -> Loc Longident -> ModuleType
ident (MkOpts {..}) a = mk (def {loc, attrs}) $ PmtyIdent a

extension :: MkOpts -> Extension -> ModuleType
extension (MkOpts {..}) a = mk (def {loc, attrs}) $ PmtyExtension a
