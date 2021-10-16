{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.CoreTypeDesc
  ( coreTypedescPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( CoreType,
    CoreTypeDesc (..),
  )
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    encloseSep,
    fillCat,
    fillSep,
    squote,
  )

coreTypedescPP :: (Pretty CoreType) => CoreTypeDesc -> Doc a
coreTypedescPP = \case
  PtypAlias _ _ -> error "TODO"
  PtypAny -> "_"
  PtypArrow {} -> error "TODO"
  PtypClass _ _ -> error "TODO"
  PtypConstr i [] -> pretty i
  PtypConstr i [x] -> fillSep [pretty x, pretty i]
  PtypConstr ctor args ->
    fillSep $ map pretty (reverse args) ++ [pretty ctor]
  PtypPoly _ _ -> error "TODO"
  PtypTuple l -> encloseSep "" "" " * " $ map pretty l
  PtypVar s -> fillCat [squote, pretty s]
  PtypVariant {} -> error "TODO"
  PtypObject _ _ -> error "TODO"
  PtypExtension _ -> error "TODO"
  PtypPackage _ -> error "TODO"

instance Pretty CoreType => Pretty CoreTypeDesc where
  pretty = coreTypedescPP
