{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.AndLetBinding.Test
  ( testStrings
  ) where

import Data.String.Interpolate

import qualified Language.OCaml.Parser.Attributes.Test as Attributes
import qualified Language.OCaml.Parser.LetBindingBody.Test as LetBindingBody
import qualified Language.OCaml.Parser.Payload.Test as Payload
import qualified Language.OCaml.Parser.PostItemAttributes.Test as PostItemAttributes

limit :: Int
limit = 10

testStrings :: [String] -> [String]
testStrings structure = []
  ++ [ [i| and #{a} #{lbb} #{pia} |]
       | a <- attributes
       , lbb <- letBindingBody
       , pia <- postItemAttributes
       ]
  where
    attributes         = take limit $ Attributes.testStrings payload
    letBindingBody     = take limit $ LetBindingBody.testStrings
    payload            = take limit $ Payload.testStrings structure
    postItemAttributes = take limit $ PostItemAttributes.testStrings payload
