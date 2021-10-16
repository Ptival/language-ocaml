{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.CoreType2.Test
  ( testStrings,
  )
where

-- import           Data.String.Interpolate

import qualified Language.OCaml.Parser.SimpleCoreTypeOrTuple.Test as SimpleCoreTypeOrTuple

testStrings :: [String]
testStrings =
  []
    ++ simpleCoreTypeOrTuple
  where
    -- TODO

    simpleCoreTypeOrTuple = SimpleCoreTypeOrTuple.testStrings
