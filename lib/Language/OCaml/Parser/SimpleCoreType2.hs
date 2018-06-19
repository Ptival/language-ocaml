module Language.OCaml.Parser.SimpleCoreType2
  ( simple_core_type2_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TypeLongident
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

simple_core_type2_P :: Parser Core_type
simple_core_type2_P = leftRecursive
  [ choice
    [ mktyp . Ptyp_var <$> try (quote_T *> ident_P)
    , mktyp . const Ptyp_any <$> underscore_T
    , try $ do
      t <- type_longident_P
      return $ mktyp $ Ptyp_constr (mkRHS t 1) []
    -- , do
    --   a <- chainl1' simple_core_type2_P _ _ -- (return $ \ a b -> Ptyp_constr (mkRHS b 2) [a])
    --   t <- type_longident_P
    --   return $ mkTyp $ Ptyp_constr (mkRHS t 2) [a]
    ]
  ]
  [ do
    t <- type_longident_P
    return $ \ x -> mktyp $ Ptyp_constr (mkRHS t 2) [x]
  ]
