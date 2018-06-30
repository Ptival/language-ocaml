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

structureTests :: [String]
structureTests =
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
  , [s|
open Lexing
open Ast
open Env

type tconstantc_module = TCModule of tfdec list
[@@deriving show, eq]
and tfdec' = { t_name:string; t_params:param list; t_rty:ctype; t_rlbl:label; t_body:tblock }
[@@deriving show, eq]
and tfdec = tfdec' pos_ast [@@deriving show, eq]
and tstm' =
  | TVarDec of string * labeled_type * texpr
  | TAssign of string * texpr
  | TArrAssign of string * texpr * texpr
  | TIf of texpr * tblock * tblock
  | TFor of string * ctype * texpr * texpr * tblock
  | TReturn of texpr
[@@deriving show, eq]
and tstm = tstm' pos_ast [@@deriving show, eq]
  |]
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Structure" $ []
  ++ map (mkParsingTest "structureP" structureP) structureTests

test :: IO ()
test = defaultMain unitTests

-- debug = debugParsing structureP (structureTests !! 9)
