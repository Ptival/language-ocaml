{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Attributes.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.Attribute.Test as Attribute

limit :: Int
limit = 10

testStrings :: [String] -> [String]
testStrings payload =
  []
    ++ [""]
    ++ [ [i| #{a} #{as} |]
         | a <- Attribute.testStrings payload,
           as <- take limit this
       ]
  where
    this = testStrings payload
