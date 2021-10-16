{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Language.OCaml.Definitions.Parsing.Longident
  ( Longident (..),
    last,
  )
where

import GHC.Generics (Generic)
import Prelude hiding (last)

data Longident
  = Lident String
  | Ldot Longident String
  | Lapply Longident Longident
  deriving (Eq, Generic, Show)

last :: Longident -> String
last = \case
  Lident s -> s
  Ldot _ s -> s
  Lapply _ _ -> error "Longident.last"
