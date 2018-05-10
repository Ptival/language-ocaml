module Language.OCaml.Definitions.Parsing.ASTTypes
  ( Arg_label(..)
  , Constant(..)
  , Loc(..)
  , Override_flag(..)
  , Rec_flag(..)
  ) where

import Language.OCaml.Definitions.Parsing.Location

data Constant
   = Const_int Int
   | Const_char Char
   | Const_string String (Maybe String)
   | Const_float String
   -- | Const_int32 Int32
   -- | Const_int64 Int64
   -- | Const_nativeint Nativeint

data Loc a = Loc
  { txt :: a
  , loc :: Location
  }
  deriving (Show)

data Override_flag
  = Override
  | Fresh
  deriving (Show)

data Rec_flag
  = Nonrecursive
  | Recursive
  deriving (Show)

data Arg_label
  = Nolabel
  | Labelled String
  | Optional String
  deriving (Show)
