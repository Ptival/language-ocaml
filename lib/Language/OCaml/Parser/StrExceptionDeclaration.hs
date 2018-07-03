module Language.OCaml.Parser.StrExceptionDeclaration
  ( strExceptionDeclarationP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SigExceptionDeclaration
import Language.OCaml.Parser.Utils.Types

strExceptionDeclarationP :: Parser Structure -> Parser (ExtensionConstructor, ())
strExceptionDeclarationP structureP = choice
  [ sigExceptionDeclarationP structureP
    -- TODO
  ]
