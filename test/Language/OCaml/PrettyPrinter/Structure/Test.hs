{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.PrettyPrinter.Structure.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Data.Text.Prettyprint.Doc
import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.PrettyPrinter.Internal
import Language.OCaml.PrettyPrinter.TestUtils

structure_tests :: [String]
structure_tests =
  [ [s|
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

type 'a binary_tree =
    | Leaf of 'a
    | Tree of 'a binary_tree * 'a binary_tree
  |]

  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.PrettyPrinter.Structure" $ []
  ++ map (mkPrettyPrinterTest
          "structure_PP"
          structure_P
          (vcat . map structure_item_PP)
         ) structure_tests

test :: IO ()
test = defaultMain unitTests

-- _foo = debugPrettyPrinter structure_P (vcat . map structure_item_PP) (structure_tests !! 0)
