{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.LetBinding.Test
  ( test
  , unitTests
  ) where

-- import Data.String.QQ
import Test.Tasty
-- import Text.Megaparsec

-- import Language.OCaml.Definitions.Parsing.Parser.LetBindings
import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

letBindingTests :: [String]
letBindingTests =
  [ "let a = b"
  , "let a : b :> c = d"
  , "let _ : a = b"
  , "let a : b = c"
  , "let f a = b"
  , "let f a = g b"
  , "let f a : t = b"
  , "let f : t = b"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.StructureItem" $ []
  ++ map (mkParsingTest "letBindingP" (letBindingP structureP)) letBindingTests

test :: IO ()
test = defaultMain unitTests

-- debug :: Int -> Either (ParseError Char Dec) LetBindings
-- debug n = debugParsing (letBindingP structureP) (letBindingTests !! n)
