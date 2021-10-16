{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.LetBinding.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.ExtAttributes.Test as ExtAttributes
import qualified Language.OCaml.Parser.LetBindingBody.Test as LetBindingBody
import qualified Language.OCaml.Parser.Payload.Test as Payload
import qualified Language.OCaml.Parser.PostItemAttributes.Test as PostItemAttributes
import qualified Language.OCaml.Parser.RecFlag.Test as RecFlag

limit :: Int
limit = 10

testStrings :: [String] -> [String]
testStrings structure =
  []
    ++ [ [i|let #{ea} #{rf} #{lbb} #{pia}|]
         | ea <- extAttributes,
           rf <- recFlag,
           lbb <- letBindingBody,
           pia <- postItemAttributes
       ]
  where
    extAttributes = take limit $ ExtAttributes.testStrings
    recFlag = take limit $ RecFlag.testStrings
    letBindingBody = take limit $ LetBindingBody.testStrings
    payload = take limit $ Payload.testStrings structure
    postItemAttributes = take limit $ PostItemAttributes.testStrings payload
