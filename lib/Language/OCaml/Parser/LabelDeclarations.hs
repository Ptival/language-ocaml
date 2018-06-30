module Language.OCaml.Parser.LabelDeclarations
  ( labelDeclarationsP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LabelDeclaration
import Language.OCaml.Parser.LabelDeclarationSemi
import Language.OCaml.Parser.Utils.Types

labelDeclarationsP :: Parser [LabelDeclaration]
labelDeclarationsP = choice
  [ do
    h <- labelDeclarationSemiP
    t <- labelDeclarationsP
    return $ h : t
  , (: []) <$> labelDeclarationSemiP
  , (: []) <$> labelDeclarationP
  ]
