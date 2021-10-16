{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.PostItemAttributes.Test
--( test
  ( testStrings,
  --, unitTests
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.PostItemAttribute.Test as PostItemAttribute

recursionLimit :: Int
recursionLimit = 10

testStrings :: [String] -> [String]
testStrings payload =
  []
    ++ [""]
    ++ [ [i|#{pia} #{pias}|]
         | pia <- PostItemAttribute.testStrings payload,
           pias <- take recursionLimit this
       ]
  where
    this = testStrings payload

-- unitTests :: TestTree
-- unitTests = testGroup "Language.OCaml.Parser.PostItemAttributes" $ []
--   ++ map (mkParsingTest "seqExprP" seqExprP) testStrings

-- test :: IO ()
-- test = defaultMain unitTests
