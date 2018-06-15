{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Implementation.Test
  ( test
  , unitTests
  ) where

-- import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
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
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Implementation" $ []
  ++ map (mkParsingTestFromFile implementation_P) files

test :: IO ()
test = defaultMain unitTests

-- foo = debugParsing implementation_P <$> readFile (prefix ++ "stdlib/arg.ml")

-- bar = debugParsing implementation_P [s|

-- let print_spec buf (key, spec, doc) =
--   if String.length doc > 0 then
--     match spec with
--     | Symbol (l, _) ->
--         bprintf buf "  %s %s%s\n" key (make_symlist "{" "|" "}" l) doc
--     | _ ->
--         bprintf buf "  %s %s\n" key doc

--   |]
