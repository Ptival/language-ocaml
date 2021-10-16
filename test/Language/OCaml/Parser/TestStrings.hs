module Language.OCaml.Parser.TestStrings
  ( attributes,
    expr,
    implementation,
    letBinding,
    payload,
    seqExpr,
    simpleExpr,
    structure,
    valIdent,
    valLongident,
  )
where

import qualified Language.OCaml.Parser.Attributes.Test as Attributes
import qualified Language.OCaml.Parser.Expr.Test as Expr
import qualified Language.OCaml.Parser.Implementation.Test as Implementation
import qualified Language.OCaml.Parser.LetBinding.Test as LetBinding
import qualified Language.OCaml.Parser.Payload.Test as Payload
import qualified Language.OCaml.Parser.SeqExpr.Test as SeqExpr
import qualified Language.OCaml.Parser.SimpleExpr.Test as SimpleExpr
import qualified Language.OCaml.Parser.Structure.Test as Structure
import qualified Language.OCaml.Parser.ValIdent.Test as ValIdent
import qualified Language.OCaml.Parser.ValLongident.Test as ValLongident

attributes :: [String]
attributes = Attributes.testStrings payload

expr :: [String]
expr = Expr.testStrings seqExpr

implementation :: [String]
implementation = Implementation.testStrings

letBinding :: [String]
letBinding = LetBinding.testStrings structure

payload :: [String]
payload = Payload.testStrings structure

seqExpr :: [String]
seqExpr = SeqExpr.testStrings

simpleExpr :: [String]
simpleExpr = SimpleExpr.testStrings seqExpr

structure :: [String]
structure = Structure.testStrings

valIdent :: [String]
valIdent = ValIdent.testStrings

valLongident :: [String]
valLongident = ValLongident.testStrings
