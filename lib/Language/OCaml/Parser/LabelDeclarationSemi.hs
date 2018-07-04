module Language.OCaml.Parser.LabelDeclarationSemi
  ( labelDeclarationSemiP
  ) where

import Data.Default
import Text.Megaparsec hiding (label)

import Language.OCaml.Definitions.Parsing.ASTHelper.Type as Type
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Label
import Language.OCaml.Parser.MutableFlag
import Language.OCaml.Parser.PolyTypeNoAttr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

labelDeclarationSemiP :: Parser LabelDeclaration
labelDeclarationSemiP = try $ do
  _mut <- mutableFlagP
  label <- labelP
  colonT
  t <- polyTypeNoAttrP
  -- TODO: attributes
  semiT
  -- TODO: attributes
  return $ Type.field def (mkRHS label 2) t -- FIXME: def
