{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.LetBindings.Test
  ( testStrings
  ) where

import Data.String.Interpolate

import qualified Language.OCaml.Parser.AndLetBinding.Test as AndLetBinding
import qualified Language.OCaml.Parser.LetBinding.Test as LetBinding

limit :: Int
limit = 10

testStrings :: [String] -> [String]
testStrings structure = []
  ++ letBinding
  ++ [ [i| #{lb} #{alb} |]
       | lb <- letBindings
       , alb <- andLetBinding
       ]
  where
    andLetBinding = take limit $ AndLetBinding.testStrings structure
    letBinding    = take limit $ LetBinding.testStrings structure
    letBindings   = take limit $ testStrings structure
