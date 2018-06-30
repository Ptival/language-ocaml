module Language.OCaml.Parser.LabelDeclarationSemi
  ( labelDeclarationSemiP
  ) where

import Text.Megaparsec hiding (label)

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Label
import Language.OCaml.Parser.MutableFlag
import Language.OCaml.Parser.PolyTypeNoAttr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

labelDeclarationSemiP :: Parser LabelDeclaration
labelDeclarationSemiP = try $ do
  mut <- mutableFlagP
  label <- labelP
  colonT
  t <- polyTypeNoAttrP
  -- TODO: attributes
  semiT
  -- TODO: attributes
  return $ field mut (mkRHS label 2) t
