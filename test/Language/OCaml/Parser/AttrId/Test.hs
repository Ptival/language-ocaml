{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.AttrId.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.SingleAttrId.Test as SingleAttrId

limit :: Int
limit = 10

testStrings :: [String]
testStrings =
  []
    ++ SingleAttrId.testStrings
    ++ [ [i| #{sai}.#{ai} ] |]
         | sai <- SingleAttrId.testStrings,
           ai <- take limit testStrings
       ]
