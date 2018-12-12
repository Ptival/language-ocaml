{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.SimpleCoreType2.Test
  ( testStrings
  ) where

import           Data.String.Interpolate
import           Prelude                          hiding (id)

import qualified Language.OCaml.Parser.Ident.Test as Ident

testStrings :: [String]
testStrings = []
  ++ [ [i| #{id} |]
       | id <- ident
       ]
  ++ ["_"]
  -- TODO
  where
    ident = Ident.testStrings
