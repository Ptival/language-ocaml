{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.ASTTypes
  ( ArgLabel(..)
  , ClosedFlag(..)
  , Constant(..)
  , Label
  , Loc(..)
  , OverrideFlag(..)
  , RecFlag(..)
  , Variance(..)
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.Location

data Constant
   = ConstInt Int
   | ConstChar Char
   | ConstString String (Maybe String)
   | ConstFloat String
   -- | ConstInt32 Int32
   -- | ConstInt64 Int64
   -- | Const_nativeint Nativeint
  deriving (Eq, Generic, Show)

data Loc a = Loc
  { txt :: a
  , loc :: Location
  }
  deriving (Generic, Show)

instance Eq a => Eq (Loc a) where
  a == b = txt a == txt b

data OverrideFlag
  = Override
  | Fresh
  deriving (Eq, Generic, Show)

data RecFlag
  = NonRecursive
  | Recursive
  deriving (Eq, Generic, Show)

data ArgLabel
  = Nolabel
  | Labelled String
  | Optional String
  deriving (Eq, Generic, Show)

data Variance
  = Covariant
  | Contravariant
  | Invariant
  deriving (Eq, Generic, Show)

data ClosedFlag
  = Closed
  | Open
  deriving (Eq, Generic, Show)

type Label = String
