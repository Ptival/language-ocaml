{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.ConstrLongident.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.ModLongident.Test as ModLongident

testStrings :: [String]
testStrings =
  []
    ++ ModLongident.testStrings
    ++ [ [i| #{ml}.(::) |]
         | ml <- ModLongident.testStrings
       ]
    ++ [ "[]",
         "()",
         "(::)",
         "false",
         "true"
       ]
