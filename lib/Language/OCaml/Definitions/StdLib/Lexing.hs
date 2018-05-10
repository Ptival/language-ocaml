{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.StdLib.Lexing
  ( Position(..)
  ) where

import GHC.Generics

data Position = Position
  { pos_fname :: String
  , pos_lnum  :: Int
  , pos_bol   :: Int
  , pos_cnum  :: Int
  }
  deriving (Eq, Generic, Show)
