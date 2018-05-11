{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.LetBinding.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.Parser.LetBindings
import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

let_binding_tests :: [String]
let_binding_tests =
  [ "let a = b"
  -- , "let f a = b"
  -- , "let f a = g b"
  -- , "let f a : t = b"
  , "let f : t = b"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.StructureItem" $ []
  ++ map (mkParsingTest "let_binding_P" (let_binding_P structure_P)) let_binding_tests

test :: IO ()
test = defaultMain unitTests

debug :: Int -> Either (ParseError Char Dec) Let_bindings
debug n = debugParsing (let_binding_P structure_P) (let_binding_tests !! n)
