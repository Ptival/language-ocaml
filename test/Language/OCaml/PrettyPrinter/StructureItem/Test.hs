{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.PrettyPrinter.StructureItem.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.PrettyPrinter.Internal
import Language.OCaml.PrettyPrinter.TestUtils

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
  , "module F = Format"
  , [s|
type a_b =
  { foo_bar: float
  ; bar: float }
  |]
  , "type c_d = {a: b; c: d}"
  , "type e_f = G of A.b * C.d_f | H | I"
  , "let a = b"
  , [s|
let some_function = function
  | Constructor _ ->
      Module.some_other_function
  | OtherConstructor _ ->
      OtherModule.other_function
  |]
  , [s|
let string_of_something = function
  | Constructor _ ->
      "some_string"
  |]
  , [s|
let multi_patterns = function
  | Constructor1 foo_bar
  | Constructor2 foo_bar
  | Constructor3 foo_bar ->
      Some foo_bar
  | _ ->
     None
  |]
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.PrettyPrinter.StructureItem" $ []
  ++ map (mkPrettyPrinterTest
          "structure_item_PP"
          (structure_item_P structure_P)
          structure_item_PP
         ) structure_item_tests

test :: IO ()
test = defaultMain unitTests

foo n =
  parseAndPrettyPrint (structure_item_P structure_P) structure_item_PP
  (structure_item_tests !! n)
