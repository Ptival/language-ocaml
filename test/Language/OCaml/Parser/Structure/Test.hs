{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Structure.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

structure_tests :: [String]
structure_tests =
  [ " "
  , "(* A *) type a = A"
  , "type a = A (* A *)"
  , "type a = A"
  , "type a = _"
  , "type a = b"
  , "type a = 'b"
  , "type a = _ ;; type a = b ;; type a = 'b"
  , [s|
type a = _
type a = 'b
type a = b
type a = |
type a = A
type a = A of t
type a = A of t | B of t
type a = A | B
type a = A | B | C
type a = | A | B | C
  |]
--   , [s|
-- let string_of_expr expr : string =
-- 	let rec f is_simple = function
-- 		| Var name -> name
-- 		| Call(fn_expr, arg_list) ->
-- 				f true fn_expr ^ "(" ^ String.concat ", " (List.map (f false) arg_list) ^ ")"
-- 		| Fun(param_list, body_expr) ->
-- 				let fun_str =
-- 					"fun " ^ String.concat " " param_list ^ " -> " ^ f false body_expr
-- 				in
-- 				if is_simple then "(" ^ fun_str ^ ")" else fun_str
--   |]
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Structure" $ []
  ++ map (mkParsingTest "structure_P" structure_P) structure_tests

test :: IO ()
test = defaultMain unitTests

debug = debugParsing structure_P (structure_tests !! 9)
