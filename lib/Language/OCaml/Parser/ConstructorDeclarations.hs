module Language.OCaml.Parser.ConstructorDeclarations
  ( constructorDeclarationsP
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.BarConstructorDeclaration
import Language.OCaml.Parser.ConstructorDeclaration
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

constructorDeclarationsP ::
  Parser Structure -> Parser CoreType -> Parser [ConstructorDeclaration]
constructorDeclarationsP structureP coreTypeP = leftRecursive
  [ (: []) <$> constructorDeclarationP'
  , (: []) <$> barConstructorDeclarationP'
  , barT *> return []
  ]
  [ (:) <$> barConstructorDeclarationP'
  ]
  where
    constructorDeclarationP'     = constructorDeclarationP     structureP coreTypeP
    barConstructorDeclarationP' = barConstructorDeclarationP structureP coreTypeP
