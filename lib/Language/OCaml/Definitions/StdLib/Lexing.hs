{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.StdLib.Lexing
  ( Position(..)
  ) where

import GHC.Generics

data Position = Position
  { posFName :: String
  , posLNum  :: Int
  , posBOL   :: Int
  , posCNum  :: Int
  }
  deriving (Eq, Generic, Show)
