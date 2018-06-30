module Language.OCaml.Parser.Internal
  ( module Language.OCaml.Parser.Tokens
  , Parser
  , attributeP
  , attributesP
  , barConstructorDeclarationP
  , constantP
  , constrIdentP
  , constrLongidentP
  , constructorArgumentsP
  , constructorDeclarationP
  , constructorDeclarationsP
  , coreTypeP
  , coreType2P
  , coreTypeCommaListP
  , coreTypelistP
  , exprP
  , generalizedConstructorArgumentsP
  , identP
  , implementationP
  , labeledSimpleExprP
  , lblExprP
  , lblExprListP
  , letBindingP
  , letBindingBodyP
  , matchCaseP
  , modLongidentP
  , openStatementP
  , patternP
  , patternNoExnP
  , postItemAttributesP
  , recordExprP
  , seqExprP
  , simpleCoreTypeP
  , simpleCoreType2P
  , simpleCoreTypeOrTupleP
  , simpleExprP
  , simpleLabeledExprListP
  , simplePatternP
  , structureP
  , structureItemP
  , typeDeclarationP
  , typeDeclarationsP
  , typeKindP
  , valIdentP
  , valLongidentP
  ) where

import           Language.OCaml.Definitions.Parsing.ASTTypes
import           Language.OCaml.Definitions.Parsing.Parser.LetBindings
import           Language.OCaml.Definitions.Parsing.ParseTree
import qualified Language.OCaml.Parser.Attribute
import qualified Language.OCaml.Parser.Attributes
import qualified Language.OCaml.Parser.BarConstructorDeclaration
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.Constant
import           Language.OCaml.Parser.ConstrIdent
import           Language.OCaml.Parser.ConstrLongident
import qualified Language.OCaml.Parser.ConstructorArguments
import qualified Language.OCaml.Parser.ConstructorDeclaration
import qualified Language.OCaml.Parser.ConstructorDeclarations
import           Language.OCaml.Parser.CoreType
import qualified Language.OCaml.Parser.CoreType2
import qualified Language.OCaml.Parser.CoreTypeCommaList
import qualified Language.OCaml.Parser.CoreTypeList
import qualified Language.OCaml.Parser.Expr
import qualified Language.OCaml.Parser.GeneralizedConstructorArguments
import           Language.OCaml.Parser.Implementation
import qualified Language.OCaml.Parser.LabeledSimpleExpr
import qualified Language.OCaml.Parser.LblExpr
import qualified Language.OCaml.Parser.LblExprList
import qualified Language.OCaml.Parser.LetBinding
import qualified Language.OCaml.Parser.LetBindingBody
import qualified Language.OCaml.Parser.MatchCase
import           Language.OCaml.Parser.ModLongident
import           Language.OCaml.Parser.OpenStatement
import           Language.OCaml.Parser.Pattern
import           Language.OCaml.Parser.PatternNoExn
import qualified Language.OCaml.Parser.PostItemAttributes
import qualified Language.OCaml.Parser.RecordExpr
import qualified Language.OCaml.Parser.SeqExpr
import qualified Language.OCaml.Parser.SimpleCoreType
import qualified Language.OCaml.Parser.SimpleCoreType2
import qualified Language.OCaml.Parser.SimpleCoreTypeOrTuple
import qualified Language.OCaml.Parser.SimpleExpr
import qualified Language.OCaml.Parser.SimpleLabeledExprList
import qualified Language.OCaml.Parser.SimplePattern
import           Language.OCaml.Parser.Structure
import qualified Language.OCaml.Parser.StructureItem
import           Language.OCaml.Parser.Tokens
import qualified Language.OCaml.Parser.TypeDeclaration
import qualified Language.OCaml.Parser.TypeDeclarations
import qualified Language.OCaml.Parser.TypeKind
import           Language.OCaml.Parser.ValIdent
import           Language.OCaml.Parser.ValLongident
import           Language.OCaml.Parser.Utils.Types

-- Tying the knots for our clients!

attributeP :: Parser (Loc String, Payload)
attributeP = Language.OCaml.Parser.Attribute.attributeP structureP

attributesP :: Parser [(Loc String, Payload)]
attributesP = Language.OCaml.Parser.Attributes.attributesP structureP

barConstructorDeclarationP :: Parser ConstructorDeclaration
barConstructorDeclarationP =
  Language.OCaml.Parser.BarConstructorDeclaration.barConstructorDeclarationP structureP coreTypeP

constructorArgumentsP :: Parser ConstructorArguments
constructorArgumentsP =
  Language.OCaml.Parser.ConstructorArguments.constructorArgumentsP coreTypeP

constructorDeclarationP :: Parser ConstructorDeclaration
constructorDeclarationP =
  Language.OCaml.Parser.ConstructorDeclaration.constructorDeclarationP structureP coreTypeP

constructorDeclarationsP :: Parser [ConstructorDeclaration]
constructorDeclarationsP =
  Language.OCaml.Parser.ConstructorDeclarations.constructorDeclarationsP structureP coreTypeP

coreType2P :: Parser CoreType
coreType2P = Language.OCaml.Parser.CoreType2.coreType2P coreTypeP

coreTypeCommaListP :: Parser [CoreType]
coreTypeCommaListP =
  Language.OCaml.Parser.CoreTypeCommaList.coreTypeCommaListP coreTypeP

coreTypelistP :: Parser [CoreType]
coreTypelistP = Language.OCaml.Parser.CoreTypeList.coreTypelistP coreTypeP

exprP :: Parser Expression
exprP = Language.OCaml.Parser.Expr.exprP structureP seqExprP

generalizedConstructorArgumentsP :: Parser (ConstructorArguments, Maybe a)
generalizedConstructorArgumentsP =
  Language.OCaml.Parser.GeneralizedConstructorArguments.generalizedConstructorArgumentsP
  coreTypeP

lblExprP :: Parser (Loc Longident, Expression)
lblExprP = Language.OCaml.Parser.LblExpr.lblExprP exprP

lblExprListP :: Parser [(Loc Longident, Expression)]
lblExprListP = Language.OCaml.Parser.LblExprList.lblExprListP exprP

labeledSimpleExprP :: Parser (ArgLabel, Expression)
labeledSimpleExprP =
  Language.OCaml.Parser.LabeledSimpleExpr.labeledSimpleExprP seqExprP exprP

letBindingP :: Parser LetBindings
letBindingP =
  Language.OCaml.Parser.LetBinding.letBindingP structureP seqExprP

letBindingBodyP :: Parser (Pattern, Expression)
letBindingBodyP =
  Language.OCaml.Parser.LetBindingBody.letBindingBodyP seqExprP

matchCaseP :: Parser Case
matchCaseP = Language.OCaml.Parser.MatchCase.matchCaseP seqExprP

postItemAttributesP :: Parser [(Loc String, Payload)]
postItemAttributesP = Language.OCaml.Parser.PostItemAttributes.postItemAttributesP structureP

recordExprP :: Parser (Maybe Expression, [(Loc Longident, Expression)])
recordExprP = Language.OCaml.Parser.RecordExpr.recordExprP exprP simpleExprP

seqExprP :: Parser Expression
seqExprP = Language.OCaml.Parser.SeqExpr.seqExprP structureP

simpleCoreTypeP :: Parser CoreType
simpleCoreTypeP = Language.OCaml.Parser.SimpleCoreType.simpleCoreTypeP coreTypeP

simpleCoreType2P :: Parser CoreType
simpleCoreType2P = Language.OCaml.Parser.SimpleCoreType2.simpleCoreType2P coreTypeP

simpleCoreTypeOrTupleP :: Parser CoreType
simpleCoreTypeOrTupleP =
  Language.OCaml.Parser.SimpleCoreTypeOrTuple.simpleCoreTypeOrTupleP coreTypeP

simpleExprP :: Parser Expression
simpleExprP = Language.OCaml.Parser.SimpleExpr.simpleExprP seqExprP exprP

simpleLabeledExprListP :: Parser [(ArgLabel, Expression)]
simpleLabeledExprListP =
  Language.OCaml.Parser.SimpleLabeledExprList.simpleLabeledExprListP seqExprP exprP

simplePatternP :: Parser Pattern
simplePatternP = Language.OCaml.Parser.SimplePattern.simplePatternP patternP

structureItemP :: Parser StructureItem
structureItemP = Language.OCaml.Parser.StructureItem.structureItemP structureP

typeDeclarationP :: Parser (RecFlag, TypeDeclaration)
typeDeclarationP = Language.OCaml.Parser.TypeDeclaration.typeDeclarationP structureP

typeDeclarationsP :: Parser (RecFlag, [TypeDeclaration])
typeDeclarationsP = Language.OCaml.Parser.TypeDeclarations.typeDeclarationsP structureP

typeKindP :: Parser (TypeKind, PrivateFlag, Maybe CoreType)
typeKindP = Language.OCaml.Parser.TypeKind.typeKindP structureP
