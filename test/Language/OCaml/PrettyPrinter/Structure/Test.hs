{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.PrettyPrinter.Structure.Test
  ( test,
    unitTests,
  )
where

import Data.String.QQ
import Language.OCaml.Parser.Generator.Parser (parseStructure)
import Language.OCaml.PrettyPrinter.Internal
import Language.OCaml.PrettyPrinter.TestUtils
import Prettyprinter
import Test.Tasty

structureTests :: [String]
structureTests =
  [ [s|
open Lexing
open Ast
open Env

type 'a binary_tree =
    | Leaf of 'a
    | Tree of 'a binary_tree * 'a binary_tree
  |]
  ]

unitTests :: TestTree
unitTests =
  testGroup "Language.OCaml.PrettyPrinter.Structure" $
    []
      ++ map
        ( mkPrettyPrinterTest
            "structurePP"
            parseStructure
            (vcat . map structureItemPP)
        )
        structureTests

test :: IO ()
test = defaultMain unitTests

-- _foo = debugPrettyPrinter structureP (vcat . map structureItemPP) (structureTests !! 0)
