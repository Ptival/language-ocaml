{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.ExpressionDesc
  ( expressionDescPP
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

expressionDescPP :: (Pretty Expression) => ExpressionDesc -> Doc a
expressionDescPP = \case
  PexpIdent i -> pretty i
  PexpCoerce _ _ _ -> error "TODO"
  PexpConstant c -> pretty c
  PexpConstraint _ _ -> error "TODO"
  PexpFun _ _ _ _ -> error "TODO"
  PexpLet r l e -> case l of
    []  -> error "TODO"
    [x] -> fillCat [ "let ", pretty r, pretty x, " in ", pretty e ]
    _   -> error "TODO"
  PexpFunction l -> case l of
    []  -> error "TODO"
    [x] -> fillSep [ "function", pretty x ]
    _   -> fillSep [ "function", nest 2 $ line <> (vcat $ map pretty l) ]
  PexpApply e l ->
    case (pexpDesc e, l) of
    {- Pairs that appear naked (without parentheses) give rise to PexpTuple
       whereas pairs that appear parenthesized show up as PexpApply of
       PexpTuple to [].  We display them back accordingly. -}
    (e'@(PexpTuple _), []) -> fillCat [ lparen, expressionDescPP e', rparen ]
    (_, _)                  -> fillCat $ pretty e : map (\ (_lbl, expr) -> pretty expr) l -- FIXME: lbl
  PexpMatch _e _l -> error "TODO"
  PexpTuple l -> encloseSep "" "" comma (map pretty l)
  PexpConstruct i e -> fillSep [ pretty i, pretty e ]
  PexpField _e _i -> error "TODO"
  PexpIfthenelse _e1 _e2 _e3 -> error "TODO"
  PexpSequence _e1 _e2 -> error "TODO"
  PexpExtension _e -> error "TODO"
  PexpTry _ _ -> error "TODO"
  PexpArray _ -> error "TODO"
  PexpWhile _ _ -> error "TODO"
  PexpUnreachable -> error "TODO"
  PexpRecord _ _ -> error "TODO"

instance Pretty Expression => Pretty ExpressionDesc where
  pretty = expressionDescPP
