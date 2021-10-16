{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.SimpleExpr.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.Constant.Test as Constant
import qualified Language.OCaml.Parser.ConstrLongident.Test as ConstrLongident
import qualified Language.OCaml.Parser.ValLongident.Test as ValLongident

testStrings :: [String] -> [String]
testStrings seqExpr =
  []
    ++ ValLongident.testStrings
    ++ Constant.testStrings
    ++ ConstrLongident.testStrings
    -- TODO: NameTag
    ++ [ [i| (#{se}) |]
         | se <- seqExpr
       ]
