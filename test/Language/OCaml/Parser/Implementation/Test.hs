{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Implementation.Test
  ( test
  , testFiles
  , testStrings
  , unitTests
  ) where

-- import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
import qualified Language.OCaml.Parser.Structure.Test as Structure
-- import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TestUtils

prefix :: FilePath
prefix = "test/Language/OCaml/Parser/Implementation/"

testStrings :: [String]
testStrings = Structure.testStrings

testFiles :: [FilePath]
testFiles = map (prefix ++)
  [ "test_00.ml"
  , "test_01.ml"
  , "test_02.ml"
  , "test_03.ml"
  , "test_04.ml"
  , "test_05.ml"
  , "test_06.ml"
  , "test_07.ml"
  , "test_08.ml"
  -- , "FaCT/tast.ml"
  -- , "FaCT/tast-00.ml"
  -- , "FaCT/tast-01.ml"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.Implementation" $ []
  ++ map (mkParsingTestFromFile implementationP) testFiles

test :: IO ()
test = defaultMain unitTests
