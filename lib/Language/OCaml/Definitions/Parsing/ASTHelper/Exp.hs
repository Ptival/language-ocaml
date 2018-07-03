{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Exp
  ( CaseOpts(..)
  , MkOpts(..)
  , case'
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

-- oops! case is a reserved Haskell lexeme
case' :: CaseOpts -> Pattern -> Expression -> Case
case' (CaseOpts {..}) lhs rhs = Case
  { pcLHS   = lhs
  , pcGuard = guard
  , pcRHS   = rhs
  }

data CaseOpts = CaseOpts
  { guard :: Maybe Expression
  }

instance Default CaseOpts where
  def = CaseOpts
    { guard  = Nothing
    }

mk :: MkOpts -> ExpressionDesc -> Expression
mk (MkOpts {..}) desc =
  Expression
  { pexpDesc       = desc
  , pexpLoc        = loc
  , pexpAttributes = attrs
  }

data MkOpts = MkOpts
  { attrs  :: [Attribute]
  , loc    :: Location
  }

instance Default MkOpts where
  def = MkOpts
    { attrs  = []
    , loc    = defaultLoc
    }
