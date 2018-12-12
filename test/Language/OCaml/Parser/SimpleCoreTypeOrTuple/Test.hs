{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.SimpleCoreTypeOrTuple.Test
  ( testStrings
  ) where

-- import           Data.String.Interpolate

import qualified Language.OCaml.Parser.SimpleCoreType.Test as SimpleCoreType

testStrings :: [String]
testStrings = []
  ++ simpleCoreType
  -- TODO
  where
    simpleCoreType = SimpleCoreType.testStrings
