module Language.OCaml.Parser.SimpleCoreType2
  ( simpleCoreType2P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.CoreTypeCommaList
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TypeLongident
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

simpleCoreType2P :: Parser CoreType -> Parser CoreType
simpleCoreType2P coreTypeP = leftRecursive
  [ choice
    [ mktyp . PtypVar <$> try (quoteT *> identP)
    , mktyp . const PtypAny <$> underscoreT
    , try $ do
      t <- typeLongidentP
      return $ mktyp $ PtypConstr (mkRHS t 1) []
    , do
      lParenT
      l <- coreTypeCommaListP coreTypeP
      rParenT
      i <- typeLongidentP
      return . mktyp $ PtypConstr (mkRHS i 4)(reverse l)
    -- , do
    --   a <- chainl1' simpleCoreType2P _ _ -- (return $ \ a b -> PtypConstr (mkRHS b 2) [a])
    --   t <- typeLongidentP
    --   return $ mkTyp $ PtypConstr (mkRHS t 2) [a]
    ]
  ]
  [ do
    t <- typeLongidentP
    return $ \ x -> mktyp $ PtypConstr (mkRHS t 2) [x]
  ]
