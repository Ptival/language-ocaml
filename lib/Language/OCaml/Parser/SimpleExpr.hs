module Language.OCaml.Parser.SimpleExpr
  ( simpleExprP
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Constant
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.ExprSemiList
import Language.OCaml.Parser.LabelLongident
import Language.OCaml.Parser.OptSemi
import Language.OCaml.Parser.RecordExpr
import Language.OCaml.Parser.ValLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

simpleExprP :: Parser Expression -> Parser Expression -> Parser Expression
simpleExprP seqExprP exprP = leftRecursive
  [ do
    i <- valLongidentP
    return . mkexp $ PexpIdent (mkRHS i 1)
  , mkexp . PexpConstant <$> constantP
  , do
    i <- constrLongidentP
    return $ mkexp $ PexpConstruct (mkRHS i 1) Nothing
  , do
    lParenT
    e <- seqExprP
    rParenT
    return $ relocExp e
  , do
    lBracketT
    l <- exprSemiListP exprP
    optSemiP
    rBracketT
    return $ relocExp $ mktailexp (rhsLoc 4) (reverse l)
  , do
    lBraceT
    (exten, fields) <- recordExprP exprP simpleExprP'
    rBraceT
    return $ mkexp $ PexpRecord fields exten
  ]
  [ do
    dotT
    i <- labelLongidentP
    return $ \ x -> mkexp $ PexpField x (mkRHS i 3)
  ]
  where
    simpleExprP' = simpleExprP seqExprP exprP
