{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.StructureItem.Test
  ( test
  , unitTests
  ) where

import Data.Text.Prettyprint.Doc
import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

structure_item_tests :: [String]
structure_item_tests =
  [ "type a = _"
  , "type a = 'b"
  , "type a = b"
  , "type a = |"
  , "type a = A"
  , "type a = A of t"
  , "type a = A of t | B of t"
  , "type a = A | B"
  , "type a = A | B | C"
  , "type a = | A | B | C"
  , [s|
type a =
  | B
  | C
  |]
  , "type a_b = {c: float; d: float}"
  , "type a_b = C of D.e_f"
  , [s|
type a =
  | B of C.d_e
  | F
  |]
  , "open A"
  , "open !A"
  , "open !A (* A *)"
  -- , "let f a : t = b"
  , "let f : t = b"
  , "let a = let b = c in d"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.StructureItem" $ []
  ++ map (mkParsingTest "structure_item_P" (structure_item_P structure_P)) structure_item_tests

test :: IO ()
test = defaultMain unitTests

debug n = debugParsing (structure_item_P structure_P) (structure_item_tests !! n)
