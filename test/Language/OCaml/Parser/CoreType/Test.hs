{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.CoreType.Test
  ( testStrings
  ) where

-- import Data.String.Interpolate

import qualified Language.OCaml.Parser.CoreTypeNoAttr.Test as CoreTypeNoAttr

testStrings :: [String]
testStrings = []
  ++ coreTypeNoAttr
  -- TODO
  where
    coreTypeNoAttr = CoreTypeNoAttr.testStrings
