module Language.OCaml.Parser.TypeKind
  ( typeKindP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ConstructorDeclarations
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.LabelDeclarations
import Language.OCaml.Parser.PrivateFlag
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

typeKindP :: Parser Structure -> Parser (TypeKind, PrivateFlag, Maybe CoreType)
typeKindP structureP = choice
  [ do
    t <- try $ do
      equalT
      coreTypeP
    return (PtypeAbstract, Public, Just t)
  , do
    cs <- try $ do
      equalT
      constructorDeclarationsP'
    return (PtypeVariant (reverse cs), Private, Nothing)
  , do
    priv <- try $ do
      equalT
      priv <- privateFlagP
      lBraceT
      return priv
    labels <- labelDeclarationsP
    rBraceT
    return (PtypeRecord labels, priv, Nothing)
  -- FIXME: commenting this out as it helps with debugging, but this is a valid parse of type kind
  -- , return (PtypeAbstract, Public, Nothing)
  ]
  where
    constructorDeclarationsP' = constructorDeclarationsP structureP coreTypeP
