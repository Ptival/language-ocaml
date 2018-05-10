{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Implementation.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Implementation
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

foo = debugParsing implementation_P [s|
(* Comment
 * on
 * multiple
 * lines
 *)

(** Different style *)

open! AModule
module F = Format

type a_b =
  { foo_bar: float
  ; bar: float }

type c_d = {a: b; c: d}

type e_f = G of A.b * C.d_f | H | I

let a = b

let some_function = function
  | Constructor _ ->
      Module.some_other_function
  | OtherConstructor _ ->
      OtherModule.other_function

let string_of_something = function
  | Constructor _ ->
      "some_string"

let multi_patterns = function
  | Constructor1 foo_bar
  | Constructor2 foo_bar
  | Constructor3 foo_bar ->
      Some foo_bar
  | _ ->
     None
|]
