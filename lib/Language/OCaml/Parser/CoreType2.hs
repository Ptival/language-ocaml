module Language.OCaml.Parser.CoreType2
  ( coreType2P
  ) where

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreTypeOrTuple
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

coreType2P :: Parser CoreType -> Parser CoreType
coreType2P coreTypeP = leftRecursive
  [ simpleCoreTypeOrTupleP coreTypeP
  -- TODO
  ]
  [ do
    minusGreaterT
    t2 <- coreType2P'
    return $ \ t1 ->
      let param = extraRHSCoreType t1 (1 :: Int) in
      mktyp $ PtypArrow Nolabel param t2
  ]
  where
    coreType2P' = coreType2P coreTypeP
