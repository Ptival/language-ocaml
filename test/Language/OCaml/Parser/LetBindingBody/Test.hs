{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.LetBindingBody.Test
  ( testStrings
  ) where

import Data.String.Interpolate

import qualified Language.OCaml.Parser.StrictBinding.Test as StrictBinding
import qualified Language.OCaml.Parser.ValIdent.Test as ValIdent

limit :: Int
limit = 100

testStrings :: [String]
testStrings = []
  ++ [ [i| #{vi} #{sb} |]
       | vi <- valIdent
       , sb <- strictBinding
       ]
  where
    valIdent      = take limit $ ValIdent.testStrings
    strictBinding = take limit $ StrictBinding.testStrings
