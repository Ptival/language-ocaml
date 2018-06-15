{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Implementation.Test
  ( test
  , unitTests
  ) where

-- import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
-- import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TestUtils

prefix :: FilePath
prefix = "test/Language/OCaml/Parser/Implementation/"

files :: [FilePath]
files = map (prefix ++)
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
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Implementation" $ []
  ++ map (mkParsingTestFromFile implementation_P) files

test :: IO ()
test = defaultMain unitTests

-- foo = debugParsing implementation_P <$> readFile (prefix ++ "FaCT/tast.ml")

-- foo = debugParsing implementation_P <$> readFile (prefix ++ "stdlib/arg.ml")

-- bar = debugParsing implementation_P [s|

-- let add_help speclist =
--   speclist @ (add1 @ add2)

--   |]
