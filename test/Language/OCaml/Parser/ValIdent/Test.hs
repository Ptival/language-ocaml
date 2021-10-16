{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.ValIdent.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.Operator.Test as Operator

testStrings :: [String]
testStrings =
  []
    ++ ["foo"]
    ++ [[i|(#{op})|] | op <- Operator.testStrings]
