{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.ConstrLongident.Test
  ( test
  , testStrings
  , unitTests
  ) where

import Data.String.Interpolate
import Test.Tasty

import Language.OCaml.Parser.Internal
import qualified Language.OCaml.Parser.ModLongident.Test as ModLongident
import Language.OCaml.Parser.TestUtils

testStrings :: [String]
testStrings = []
  ++ ModLongident.testStrings
  ++ [ [i| #{ml}.(::) |]
       | ml <- ModLongident.testStrings
     ]
  ++ [ "[]"
     , "()"
     , "(::)"
     , "false"
     , "true"
     ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ConstrLongident" $ []
  ++ map (mkParsingTest "constrLongidentP" constrLongidentP) testStrings

test :: IO ()
test = defaultMain unitTests
