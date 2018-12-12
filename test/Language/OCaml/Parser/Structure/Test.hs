{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Structure.Test
  ( testStrings
  ) where

import           Data.String.Interpolate

import qualified Language.OCaml.Parser.Payload.Test as Payload
import qualified Language.OCaml.Parser.PostItemAttributes.Test as PostItemAttributes
import qualified Language.OCaml.Parser.SeqExpr.Test as SeqExpr
import qualified Language.OCaml.Parser.StructureTail.Test as StructureTail

limit :: Int
limit = 4

testStrings :: [String]
testStrings = []
  ++ [ [i|#{se} #{pia} #{st}|]
       | se  <- seqExpr
       , pia <- postItemAttributes
       , st  <- structureTail
       ]
  where
    payload            = take limit $ Payload.testStrings structure
    postItemAttributes = take limit $ PostItemAttributes.testStrings payload
    seqExpr            = take limit $ SeqExpr.testStrings
    structure          = take limit $ testStrings
    structureTail      = take limit $ StructureTail.testStrings structure
