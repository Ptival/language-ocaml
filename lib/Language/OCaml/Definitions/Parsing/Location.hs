{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Location
  ( Location(..)
  , none
  ) where

import GHC.Generics

import Language.OCaml.Definitions.StdLib.Lexing

data Location = Location
  { loc_start :: Position
  , loc_end   :: Position
  , loc_ghost :: Bool
  }
  deriving (Eq, Generic, Show)

in_file :: String -> Location
in_file name =
  Location
  { loc_start = loc
  , loc_end   = loc
  , loc_ghost = True
  }
  where
    loc = Position
          { pos_fname = name
          , pos_lnum = 1
          , pos_bol = 0
          , pos_cnum = -1
          }

none :: Location
none = in_file "__none__"
