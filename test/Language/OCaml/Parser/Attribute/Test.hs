{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Attribute.Test
  ( testStrings
  ) where

import Data.String.Interpolate

import qualified Language.OCaml.Parser.AttrId.Test as AttrId

testStrings :: [String] -> [String]
testStrings payload = []
  ++ [ [i| [@@#{ai} #{p}] |]
       | ai <- AttrId.testStrings
       , p <- payload
       ]
