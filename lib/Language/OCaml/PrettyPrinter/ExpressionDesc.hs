{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.ExpressionDesc
  ( expressionDescPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ArgLabel        ()
import Language.OCaml.PrettyPrinter.Case            ()
import Language.OCaml.PrettyPrinter.Constant        ()
import Language.OCaml.PrettyPrinter.Loc             ()
import Language.OCaml.PrettyPrinter.Longident       ()
import Language.OCaml.PrettyPrinter.RecFlag         ()
import Language.OCaml.PrettyPrinter.ValueBinding    ()

expressionDescPP :: (Pretty Case, Pretty Expression) => ExpressionDesc -> Doc a
expressionDescPP = \case

  PexpApply e l ->
    encloseSep "" "" " " $ pretty e : map (\ (_lbl, expr) -> prettyExpr expr) l -- FIXME: lbl
    where
      prettyExpr expr@(Expression { pexpDesc }) = case pexpDesc of
        PexpApply _ _ -> parens $ pretty expr
        _ -> pretty expr

  PexpArray _ -> error "TODO"
  PexpAssert _ -> error "TODO"
  PexpCoerce _ _ _ -> error "TODO"
  PexpConstant c -> pretty c
  PexpConstraint _ _ -> error "TODO"

  PexpConstruct i e ->
    -- NOTE: adding space so as to print `Some x` rather than `Somex` when parentheses are omitted
    fillCat [ pretty i, space, pretty e ]

  PexpExtension _e -> error "TODO"
  PexpField _e _i -> error "TODO"
  PexpFor _ _ _ _ _ -> error "TODO"

  PexpFun _argLabel _ patt expr -> fillCat [ "fun ", pretty patt, " -> ", pretty expr ]

  PexpFunction l -> case l of
    []  -> error "TODO"
    [x] -> fillSep [ "function", pretty x ]
    _   -> fillSep [ "function", nest 2 $ line <> (vcat $ map pretty l) ]

  PexpIdent i -> pretty i
  PexpIfThenElse _e1 _e2 _e3 -> error "TODO"
  PexpLazy _ -> error "TODO"

  PexpLet r l e -> case l of
    []  -> error "TODO"
    [x] -> fillCat [ "let ", pretty r, pretty x, " in ", pretty e ]
    _   -> error "TODO"

  PexpLetException _ _ -> error "TODO"
  PexpLetModule _ _ _ -> error "TODO"

  PexpMatch discriminee branches -> vcat $ pretty discriminee : map pretty branches

  PexpNew _ -> error "TODO"
  PexpNewType _ _ -> error "TODO"
  PexpObject _ -> error "TODO"
  PexpOpen _ _ _ -> error "TODO"
  PexpOverride _ -> error "TODO"
  PexpPack _ -> error "TODO"
  PexpPoly _ _ -> error "TODO"
  PexpRecord _ _ -> error "TODO"
  PexpSend _ _ -> error "TODO"
  PexpSequence _e1 _e2 -> error "TODO"
  PexpSetField _ _ _ -> error "TODO"
  PexpSetInstVar _ _ -> error "TODO"
  PexpTry _ _ -> error "TODO"

  PexpTuple l -> encloseSep "(" ")" ", " (map pretty l)

  PexpUnreachable -> error "TODO"
  PexpVariant _ _ -> error "TODO"
  PexpWhile _ _ -> error "TODO"

instance Pretty Expression => Pretty ExpressionDesc where
  pretty = expressionDescPP
