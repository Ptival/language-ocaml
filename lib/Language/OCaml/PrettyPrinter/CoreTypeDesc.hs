{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.CoreTypeDesc
  ( coreTypedescPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()

coreTypedescPP :: (Pretty CoreType) => CoreTypeDesc -> Doc a
coreTypedescPP = \case
  PtypAlias _ _ -> error "TODO"
  PtypAny -> "_"
  PtypArrow _ _ _ -> error "TODO"
  PtypClass _ _ -> error "TODO"
  PtypConstr i [] -> pretty i
  PtypConstr i [x] -> fillSep $ [ pretty x, pretty i ]
  PtypConstr _ _ -> error "TODO"
  PtypPoly _ _ -> error "TODO"
  PtypTuple l -> fillSep $ map pretty l
  PtypVar s -> fillCat [ squote, pretty s ]
  PtypVariant _ _ _ -> error "TODO"
  PtypObject _ _ -> error "TODO"
  PtypExtension _ -> error "TODO"
  PtypPackage _ -> error "TODO"

instance Pretty CoreType => Pretty CoreTypeDesc where
  pretty = coreTypedescPP
