{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Tokens.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Text.Megaparsec
import Test.Tasty

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TestUtils

l_ident_tests :: [String]
l_ident_tests = [ "a", "a " ]

u_ident_tests :: [String]
u_ident_tests = [ "A", "A " ]

star_tests :: [String]
star_tests = [ "*", "* ", " *", " * " ]

string_tests :: [String]
string_tests =
  [ [s|"foo"|]
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Tokens" $ []
  ++ map (mkParsingTest "string_T" string_T) string_tests

test :: IO ()
test = defaultMain unitTests

foo = debugParsing (string_T *> string_T) [s|"foo" "bar"|]
