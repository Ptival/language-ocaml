{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.PatternDesc
  ( patternDescPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ModuleBinding   ()
import Language.OCaml.PrettyPrinter.OpenDescription ()
import Language.OCaml.PrettyPrinter.TypeDeclaration ()

patternDescPP :: Pretty Pattern => PatternDesc -> Doc a
patternDescPP = \case

  PpatAlias _p _v -> error "TODO"

  PpatAny -> "_"

  PpatArray _ -> error "TODO"
  PpatConstant _c -> error "TODO"
  PpatConstraint _ _ -> error "TODO"

  PpatConstruct i p -> case p of
    Nothing -> pretty i
    Just p' ->
      -- NOTE: adding space here so as to print `Constructor _` rather than `Constructor_`
      fillCat [ pretty i, space, pretty p' ]

  PpatException _ -> error "TODO"
  PpatExtension _ -> error "TODO"
  PpatInterval _ _ -> error "TODO"
  PpatLazy _ -> error "TODO"
  PpatOpen _ _ -> error "TODO"

  PpatOr p1 p2 -> vcat [ pretty p1, fillSep [ pipe, pretty p2 ] ]

  PpatRecord _ _ -> error "TODO"

  PpatTuple l -> encloseSep "(" ")" "," $ map pretty l

  PpatType _ -> error "TODO"
  PpatUnpack _ -> error "TODO"

  PpatVar v -> pretty v

  PpatVariant _ _ -> error "TODO"

instance Pretty Pattern => Pretty PatternDesc where
  pretty = patternDescPP
