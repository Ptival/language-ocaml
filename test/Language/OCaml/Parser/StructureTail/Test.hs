{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.StructureTail.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.StructureItem.Test as StructureItem

limit :: Int
limit = 10

testStrings :: [String] -> [String]
testStrings structure =
  []
    ++ [""]
    ++ [ [i| ;; #{s} |]
         | s <- structure
       ]
    ++ [ [i| #{si} #{st} |]
         | si <- structureItem,
           st <- structureTail
       ]
  where
    structureItem = take limit $ StructureItem.testStrings structure
    structureTail = take limit $ testStrings structure
