module Language.OCaml.Parser.CoreType2
  ( core_type2_P
  ) where

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreTypeOrTuple
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

core_type2_P :: Parser Core_type -> Parser Core_type
core_type2_P core_type_P = leftRecursive
  [ simple_core_type_or_tuple_P core_type_P
  -- TODO
  ]
  [ do
    minus_greater_T
    t2 <- core_type2_P'
    return $ \ t1 ->
      let param = extra_rhs_core_type t1 (1 :: Int) in
      mktyp $ Ptyp_arrow Nolabel param t2
  ]
  where
    core_type2_P' = core_type2_P core_type_P
