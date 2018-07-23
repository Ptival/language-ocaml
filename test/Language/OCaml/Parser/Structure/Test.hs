{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Structure.Test
  ( test
  , testStrings
  , unitTests
  ) where

import           Data.String.Interpolate
import           Test.Tasty

import           Language.OCaml.Parser.Internal
import qualified Language.OCaml.Parser.Payload.Test as Payload
import qualified Language.OCaml.Parser.PostItemAttributes.Test as PostItemAttributes
import qualified Language.OCaml.Parser.SeqExpr.Test as SeqExpr
import qualified Language.OCaml.Parser.StructureTail.Test as StructureTail
import           Language.OCaml.Parser.TestUtils

limit :: Int
limit = 2

testStrings :: [String]
testStrings = []
  ++ [ [i|#{se} #{pia} #{st}|]
       | se <- seqExpr
       , pia <- postItemAttributes
       , st <- structureTail
       ]
  where
    payload            = take limit $ Payload.testStrings structure
    postItemAttributes = take limit $ PostItemAttributes.testStrings payload
    seqExpr            = take limit $ SeqExpr.testStrings
    structure          = take limit $ testStrings
    structureTail      = take limit $ StructureTail.testStrings structure

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Structure" $ []
  ++ map (mkParsingTest "structureP" structureP) testStrings

test :: IO ()
test = defaultMain unitTests

-- debug = debugParsing structureP (structureTests !! 9)
