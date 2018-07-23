{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.PostItemAttribute.Test
  --( test
  ( testStrings
  --, unitTests
  ) where

import Data.String.Interpolate
-- import Test.Tasty

-- import Language.OCaml.Parser.Internal
import qualified Language.OCaml.Parser.AttrId.Test as AttrId
-- import Language.OCaml.Parser.TestUtils

limit :: Int
limit = 3

testStrings :: [String] -> [String]
testStrings payload = []
  ++ [ [i| [@@ #{ai} #{p} ] |]
       | ai <- AttrId.testStrings
       , p <- take limit payload
       ]

-- unitTests :: TestTree
-- unitTests = testGroup "Language.OCaml.Parser.PostItemAttribute" $ []
--   ++ map (mkParsingTest "seqExprP" seqExprP) testStrings

-- test :: IO ()
-- test = defaultMain unitTests
