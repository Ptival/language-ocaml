{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.ASTTypes
  ( Arg_label(..)
  , Closed_flag(..)
  , Constant(..)
  , Loc(..)
  , Override_flag(..)
  , Rec_flag(..)
  , Variance(..)
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.Location

data Constant
   = Const_int Int
   | Const_char Char
   | Const_string String (Maybe String)
   | Const_float String
   -- | Const_int32 Int32
   -- | Const_int64 Int64
   -- | Const_nativeint Nativeint
  deriving (Eq, Generic, Show)

data Loc a = Loc
  { txt :: a
  , loc :: Location
  }
  deriving (Generic, Show)

instance Eq a => Eq (Loc a) where
  a == b = txt a == txt b

data Override_flag
  = Override
  | Fresh
  deriving (Eq, Generic, Show)

data Rec_flag
  = Nonrecursive
  | Recursive
  deriving (Eq, Generic, Show)

data Arg_label
  = Nolabel
  | Labelled String
  | Optional String
  deriving (Eq, Generic, Show)

data Variance
  = Covariant
  | Contravariant
  | Invariant
  deriving (Eq, Generic, Show)

data Closed_flag
  = Closed
  | Open
  deriving (Eq, Generic, Show)
