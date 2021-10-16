{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.SimpleCoreType2.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.Ident.Test as Ident
import Prelude hiding (id)

testStrings :: [String]
testStrings =
  []
    ++ [ [i| #{id} |]
         | id <- ident
       ]
    ++ ["_"]
  where
    -- TODO

    ident = Ident.testStrings
