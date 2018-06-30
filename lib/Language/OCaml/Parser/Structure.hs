module Language.OCaml.Parser.Structure
  ( structureP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.SeqExpr
import Language.OCaml.Parser.StructureItem
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Utils
import Language.OCaml.Parser.Utils.Types

structureP :: Parser Structure
structureP = ocamlSpace *> choice
  [ do
    e <- seqExprP structureP
    a <- postItemAttributesP structureP
    s <- structureTailP
    return $ textStr 1 ++ mkstrexp e a : s
  , structureTailP
  ]
  where
    structureTailP :: Parser [StructureItem]
    structureTailP = choice
      [ do
        (i, t) <- try $ do
          i <- structureItemP structureP
          t <- structureTailP
          return (i, t)
        return $ textStr 1 ++ i : t
      , do
        try $ semiSemiT
        s <- structureP
        return $ textStr 1 ++ s
      , ocamlSpace *> return []
      ]
