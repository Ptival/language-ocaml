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

instance Ord Position where
  p1 <= p2 = posLNum p1 <= posLNum p2 || (posLNum p1 == posLNum p2 && posCNum p1 <= posCNum p2)
