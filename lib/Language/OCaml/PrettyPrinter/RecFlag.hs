{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.RecFlag
  ( rec_flag_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes

rec_flag_PP :: Rec_flag -> Doc a
rec_flag_PP = \case
  Recursive    -> "rec "
  NonRecursive -> ""

instance Pretty Rec_flag where
  pretty = rec_flag_PP
