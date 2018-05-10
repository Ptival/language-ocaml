{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Implementation.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
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

foo = debugParsing implementation_P [s|
type name = string

type expr =
	| Var of name                           (* variable *)
	| Call of expr * expr list              (* application *)
	| Fun of name list * expr               (* abstraction *)
	| Let of name * expr * expr             (* let *)
	| RecordSelect of expr * name           (* selecting value of label: `r.a` *)
	| RecordExtend of name * expr * expr    (* extending a record: `{a = 1, b = 2 | r}` *)
	| RecordRestrict of expr * name         (* deleting a label: `{r - a}` *)
	| RecordEmpty                           (* empty record: `{}` *)

type id = int
type level = int

type ty =
	| TConst of name                    (* type constant: `int` or `bool` *)
	| TApp of ty * ty list              (* type application: `list[int]` *)
	| TArrow of ty list * ty            (* function type: `(int, int) -> int` *)
	| TVar of tvar ref                  (* type variable *)
	| TRecord of row                    (* record type: `{<...>}` *)
	| TRowEmpty                         (* empty row: `<>` *)
	| TRowExtend of name * ty * row     (* row extension: `<a = _ | ...>` *)

and row = ty    (* the kind of rows - empty row, row variable, or row extension *)

and tvar =
	| Unbound of id * level
	| Link of ty
	| Generic of id

|]
