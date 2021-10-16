{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.SimpleCoreType.Test
  ( testStrings,
  )
where

-- import           Data.String.Interpolate

import qualified Language.OCaml.Parser.SimpleCoreType2.Test as SimpleCoreType2

testStrings :: [String]
testStrings =
  []
    ++ simpleCoreType2
  where
    -- TODO

    simpleCoreType2 = SimpleCoreType2.testStrings
