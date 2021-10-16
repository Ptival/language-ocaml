{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.LetBindingBody.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.CoreType.Test as CoreType
import qualified Language.OCaml.Parser.SeqExpr.Test as SeqExpr
import qualified Language.OCaml.Parser.SimplePatternNotIdent.Test as SimplePatternNotIdent
import qualified Language.OCaml.Parser.StrictBinding.Test as StrictBinding
import qualified Language.OCaml.Parser.ValIdent.Test as ValIdent

limit :: Int
limit = 100

testStrings :: [String]
testStrings =
  []
    ++ [ [i|#{vi} #{sb}|]
         | vi <- valIdent,
           sb <- strictBinding
       ]
    -- TODO
    ++ [ [i|#{spni} : #{ct} = #{se}|]
         | spni <- simplePatternNotIdent,
           ct <- coreType,
           se <- seqExpr
       ]
  where
    coreType = take limit $ CoreType.testStrings
    seqExpr = take limit $ SeqExpr.testStrings
    simplePatternNotIdent = take limit $ SimplePatternNotIdent.testStrings
    strictBinding = take limit $ StrictBinding.testStrings
    valIdent = take limit $ ValIdent.testStrings
