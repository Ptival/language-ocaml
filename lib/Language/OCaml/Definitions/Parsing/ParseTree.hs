{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute
  , Attributes
  , Case(..)
  , Constant(..)
  , ConstructorArguments(..)
  , ConstructorDeclaration(..)
  , CoreType(..)
  , CoreTypeDesc(..)
  , Expression(..)
  , ExpressionDesc(..)
  , Extension
  , ExtensionConstructor(..)
  , ExtensionConstructorKind(..)
  , LabelDeclaration(..)
  , Longident(..)
  , ModuleBinding(..)
  , ModuleExpr(..)
  , ModuleExprDesc(..)
  , MutableFlag(..)
  , OpenDescription(..)
  , Pattern(..)
  , PatternDesc(..)
  , Payload(..)
  , PrivateFlag(..)
  , RowField(..)
  , Signature
  , SignatureItem(..)
  , SignatureItemDesc(..)
  , Structure
  , StructureItem(..)
  , StructureItemDesc(..)
  , TypeDeclaration(..)
  , TypeException(..)
  , TypeExtension(..)
  , TypeKind(..)
  , ValueBinding(..)
  , ValueDescription(..)
  , none
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.ASTTypes hiding (Constant)
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.Longident

data Constant
  = PconstInteger String (Maybe Char)
  | PconstChar Char
  | PconstString String (Maybe String)
  | PconstFloat String (Maybe Char)
  deriving (Eq, Generic, Show)

type Attribute = (Loc String, Payload)
type Extension = (Loc String, Payload)
type Attributes = [Attribute]

data Payload
  = PStr Structure
  | PSig Signature
  | PTyp CoreType
  | PPat Pattern (Maybe Expression)
  deriving (Eq, Generic, Show)

data CoreType = CoreType
  { ptypDesc       :: CoreTypeDesc
  , ptypLoc        :: Location
  , ptypAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data CoreTypeDesc
  = PtypAny
  | PtypVar String
  | PtypArrow ArgLabel CoreType CoreType
  | PtypTuple [CoreType]
  | PtypConstr (Loc Longident) [CoreType]
  -- | PtypObject of object_field list * Asttypes.closedFlag
  | PtypClass (Loc Longident) [CoreType]
  | PtypAlias CoreType String
  | PtypVariant [RowField] ClosedFlag (Maybe [Label])
  | PtypPoly [Loc String] CoreType
  -- | PtypPackage of packageType
  -- | Ptyp_extension of extension
  deriving (Eq, Generic, Show)

data ConstructorArguments
  = PcstrTuple [CoreType]
  | PcstrRecord [LabelDeclaration]
  deriving (Eq, Generic, Show)

data PrivateFlag
  = Private
  | Public
  deriving (Eq, Generic, Show)

data TypeDeclaration = TypeDeclaration
  { ptypeName :: Loc String
  , ptypeParams :: [(CoreType, Variance)]
  , ptypeCstrs :: [(CoreType, CoreType, Location)]
  , ptypeKind :: TypeKind
  , ptypePrivate :: PrivateFlag
  , ptypeManifest :: Maybe CoreType
  , ptypeAttributes :: Attributes
  , ptypeLoc :: Location
  }
  deriving (Eq, Generic, Show)

data TypeKind
  = PtypeAbstract
  | PtypeVariant [ConstructorDeclaration]
  | PtypeRecord [LabelDeclaration]
  | PtypeOpen
  deriving (Eq, Generic, Show)

data LabelDeclaration = LabelDeclaration
  { pldName       :: Loc String
  , pldMutable    :: MutableFlag
  , pldType       :: CoreType
  , pldLoc        :: Location
  , pldAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ConstructorDeclaration = ConstructorDeclaration
  { pcdName       :: Loc String
  , pcdArgs       :: ConstructorArguments
  , pcdRes        :: Maybe CoreType
  , pcdLoc        :: Location
  , pcdAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data MutableFlag
  = Immutable
  | Mutable
  deriving (Eq, Generic, Show)

type Structure = [StructureItem]

data StructureItem = StructureItem
  { pstrDesc :: StructureItemDesc
  , pstrLoc  :: Location
  }
  deriving (Eq, Generic, Show)

data StructureItemDesc
  = PstrEval Expression Attributes
  | PstrValue RecFlag [ValueBinding]
  | PstrPrimitive ValueDescription
  | PstrType RecFlag [TypeDeclaration]
  -- | PstrTypext type_extension
  | PstrException ExtensionConstructor
  | PstrModule ModuleBinding
  -- | PstrRecmodule moduleBinding list
  -- | PstrModtype moduleTypeDeclaration
  | PstrOpen OpenDescription
  -- | PstrClass classDeclaration list
  -- | PstrClassType classTypeDeclaration list
  -- | PstrInclude includeDeclaration
  | PstrAttribute Attribute
  | PstrExtension Extension Attributes
  deriving (Eq, Generic, Show)

data Expression = Expression
  { pexpDesc       :: ExpressionDesc
  , pexpLoc        :: Location
  , pexpAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ExpressionDesc
  = PexpIdent (Loc Longident)
  | PexpConstant Constant
  | PexpLet RecFlag [ValueBinding] Expression
  | PexpFunction [Case]
  | PexpFun ArgLabel (Maybe Expression) Pattern Expression
  | PexpApply Expression [(ArgLabel, Expression)]
  | PexpMatch Expression [Case]
  | PexpTry Expression [Case]
  | PexpTuple [Expression]
  | PexpConstruct (Loc Longident) (Maybe Expression)
  -- | PexpVariant Asttypes.label * expression option
  | PexpRecord [(Loc Longident, Expression)] (Maybe Expression)
  | PexpField Expression (Loc Longident)
  | PexpSetField Expression (Loc Longident) Expression
  | PexpArray [Expression]
  | PexpIfThenElse Expression Expression (Maybe Expression)
  | PexpSequence Expression Expression
  | PexpWhile Expression Expression
  | PexpFor Pattern Expression Expression DirectionFlag Expression
  | PexpConstraint Expression CoreType
  | PexpCoerce Expression (Maybe CoreType) CoreType
  -- | Pexp_send expression * Asttypes.label Asttypes.loc
  -- | Pexp_new Longident.t Asttypes.loc
  -- | Pexp_setinstvar Asttypes.label Asttypes.loc * expression
  -- | Pexp_override (Asttypes.label Asttypes.loc * expression) list
  -- | PexpLetmodule string Asttypes.loc * moduleExpr * expression
  -- | PexpLetexception extensionConstructor * expression
  -- | PexpAssert expression
  -- | Pexp_lazy expression
  -- | PexpPoly expression * coreType option
  -- | PexpObject classStructure
  -- | Pexp_newtype string Asttypes.loc * expression
  -- | PexpPack moduleExpr
  -- | Pexp_open Asttypes.overrideFlag * Longident.t Asttypes.loc * expression
  | PexpExtension Extension
  | PexpUnreachable
  deriving (Eq, Generic, Show)

type Signature = [SignatureItem]

data SignatureItem = SignatureItem
  { psigDesc :: SignatureItemDesc
  , psigLoc  :: Location
  }
  deriving (Eq, Generic, Show)

data SignatureItemDesc
  = PsigValue ValueDescription
  -- | PsigType Asttypes.recFlag * typeDeclaration list
  -- | PsigTypext type_extension
  -- | Psig_exception extensionConstructor
  -- | PsigModule moduleDeclaration
  -- | PsigRecmodule moduleDeclaration list
  -- | PsigModtype moduleTypeDeclaration
  -- | Psig_open openDescription
  -- | PsigInclude includeDescription
  -- | PsigClass classDescription list
  -- | PsigClassType classTypeDeclaration list
  -- | PsigAttribute attribute
  -- | Psig_extension extension * attributes
  deriving (Eq, Generic, Show)

data ValueDescription = ValueDescription
  { pvalName       :: Loc String
  , pvalType       :: CoreType
  , pvalPrim       :: [String]
  , pvalAttributes :: Attributes
  , pvalLoc        :: Location
  }
  deriving (Eq, Generic, Show)

data Pattern = Pattern
  { ppatDesc       :: PatternDesc
  , ppatLoc        :: Location
  , ppatAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data PatternDesc
  = PpatAny
  | PpatVar (Loc String)
  | PpatAlias Pattern (Loc String)
  | PpatConstant Constant
  -- | PpatInterval constant * constant
  | PpatTuple [Pattern]
  | PpatConstruct (Loc Longident) (Maybe Pattern)
  -- | PpatVariant Asttypes.label * pattern option
  | PpatRecord [(Loc Longident, Pattern)] ClosedFlag
  | PpatArray [Pattern]
  | PpatOr Pattern Pattern
  | PpatConstraint Pattern CoreType
  -- | PpatType Longident.t Asttypes.loc
  -- | PpatLazy pattern
  -- | PpatUnpack string Asttypes.loc
  -- | PpatException pattern
  -- | PpatExtension extension
  -- | PpatOpen Longident.t Asttypes.loc * pattern
  deriving (Eq, Generic, Show)

data OpenDescription = OpenDescription
  { popenLid        :: Loc Longident
  , popenOverride   :: OverrideFlag
  , popenLoc        :: Location
  , popenAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ModuleExpr = ModuleExpr
  { pmodDesc       :: ModuleExprDesc
  , pmodLoc        :: Location
  , pmodAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ModuleExprDesc
  = PmodIdent (Loc Longident)
  -- | PmodStructure of structure
  -- | PmodFunctor of string Asttypes.loc * moduleType option * moduleExpr
  -- | PmodApply of moduleExpr * moduleExpr
  -- | PmodConstraint of moduleExpr * moduleType
  -- | Pmod_unpack of expression
  -- | Pmod_extension of extension
  deriving (Eq, Generic, Show)

data ModuleBinding = ModuleBinding
  { pmbName       :: Loc String
  , pmbExpr       :: ModuleExpr
  , pmbAttributes :: Attributes
  , pmbLoc        :: Location
  }
  deriving (Eq, Generic, Show)

data ValueBinding = ValueBinding
  { pvbPat        :: Pattern
  , pvbExpr       :: Expression
  , pvbAttributes :: Attributes
  , pvbLoc        :: Location
  }
  deriving (Eq, Generic, Show)

data Case = Case
  { pcLHS   :: Pattern
  , pcGuard :: Maybe Expression
  , pcRHS   :: Expression
  }
  deriving (Eq, Generic, Show)

data ExtensionConstructor = ExtensionConstructor
  { pextName        :: Loc String
  , pextKind        :: ExtensionConstructorKind
  , pextLoc         :: Location
  , pextAttributes  :: Attributes -- C of ... [@id1] [@id2]
  }
  deriving (Eq, Generic, Show)

data ExtensionConstructorKind
  = PextDecl ConstructorArguments (Maybe CoreType)
  | PextRebind (Loc Longident)
  deriving (Eq, Generic, Show)

data TypeExtension = TypeExtension
  { ptyextPath         :: Loc Longident
  , ptyextParams       :: [(CoreType, Variance)]
  , ptyextConstructors :: [ExtensionConstructor]
  , ptyextPrivate      :: PrivateFlag
  , ptyextAttributes   :: Attributes
  }
  deriving (Eq, Generic, Show)

data TypeException = TypeException
  { ptyexnConstructor :: ExtensionConstructor
  , ptyexnAttributes  :: Attributes
  }
  deriving (Eq, Generic, Show)

data RowField
  = Rtag (Loc Label) Attributes Bool
  | Rinherit CoreType
  deriving (Eq, Generic, Show)
