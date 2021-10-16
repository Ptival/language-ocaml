{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.OpenDescription
  ( openDescriptionPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( OpenDescription (popenLid, popenOverride),
  )
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Language.OCaml.PrettyPrinter.OverrideFlag ()
import Prettyprinter (Doc, Pretty (pretty), fillCat, space)

openDescriptionPP :: OpenDescription -> Doc a
openDescriptionPP d =
  fillCat
    [ "open",
      space,
      pretty $ popenOverride d,
      pretty $ popenLid d
    ]

instance Pretty OpenDescription where
  pretty = openDescriptionPP
