{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.CoreTypeNoAttr.Test
  ( testStrings
  ) where

import           Data.String.Interpolate
import           Prelude                              hiding (id)

import qualified Language.OCaml.Parser.CoreType2.Test as CoreType2
import qualified Language.OCaml.Parser.Ident.Test     as Ident

testStrings :: [String]
testStrings = []
  ++ coreType2
  ++ [ [i| #{ct2} as '#{id} |]
       | ct2 <- coreType2
       , id  <- ident
       ]
  where
    coreType2 = CoreType2.testStrings
    ident     = Ident.testStrings
