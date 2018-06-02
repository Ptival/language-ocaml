{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.ExpressionDesc
  ( expression_desc_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ArgLabel ()
import Language.OCaml.PrettyPrinter.Case ()
import Language.OCaml.PrettyPrinter.Constant ()
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Language.OCaml.PrettyPrinter.RecFlag ()
import Language.OCaml.PrettyPrinter.ValueBinding ()

expression_desc_PP :: (Pretty Expression) => Expression_desc -> Doc a
expression_desc_PP = \case
  Pexp_ident i -> pretty i
  Pexp_coerce _ _ _ -> error "TODO"
  Pexp_constant c -> pretty c
  Pexp_constraint _ _ -> error "TODO"
  Pexp_fun _ _ _ _ -> error "TODO"
  Pexp_let r l e -> case l of
    []  -> error "TODO"
    [x] -> fillCat [ "let ", pretty r, pretty x, " in ", pretty e ]
    _   -> error "TODO"
  Pexp_function l -> case l of
    []  -> error "TODO"
    [x] -> fillSep [ "function", pretty x ]
    _   -> fillSep [ "function", nest 2 $ line <> (vcat $ map pretty l) ]
  Pexp_apply e l ->
    case (pexp_desc e, l) of
    {- Pairs that appear naked (without parentheses) give rise to Pexp_tuple
       whereas pairs that appear parenthesized show up as Pexp_apply of
       Pexp_tuple to [].  We display them back accordingly. -}
    (e'@(Pexp_tuple _), []) -> fillCat [ lparen, expression_desc_PP e', rparen ]
    (_, _)                  -> fillCat $ pretty e : map (\ (_lbl, expr) -> pretty expr) l -- FIXME: lbl
  Pexp_match _e _l -> error "TODO"
  Pexp_tuple l -> encloseSep "" "" comma (map pretty l)
  Pexp_construct i e -> fillSep [ pretty i, pretty e ]
  Pexp_field _e _i -> error "TODO"
  Pexp_ifthenelse _e1 _e2 _e3 -> error "TODO"
  Pexp_sequence _e1 _e2 -> error "TODO"
  Pexp_extension _e -> error "TODO"

instance Pretty Expression => Pretty Expression_desc where
  pretty = expression_desc_PP
