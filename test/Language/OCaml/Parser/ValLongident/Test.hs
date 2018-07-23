{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.ValLongident.Test
  ( testStrings
  ) where

import Data.String.Interpolate

import qualified Language.OCaml.Parser.ModLongident.Test as ModLongident
import qualified Language.OCaml.Parser.ValIdent.Test as ValIdent

testStrings :: [String]
testStrings = []
  ++ ValIdent.testStrings
  ++ [ [i| #{m}.#{v} |]
         | m <- ModLongident.testStrings
         , v <- ValIdent.testStrings
     ]
