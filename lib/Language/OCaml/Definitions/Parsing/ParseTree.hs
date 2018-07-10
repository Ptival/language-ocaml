{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute
  , Attributes
  , Case(..)
  , ClassExpr(..)
  , ClassField(..)
  , ClassSignature(..)
  , ClassStructure(..)
  , ClassType(..)
  , ClassTypeField(..)
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
  , ModuleType(..)
  , ModuleTypeDesc(..)
  , MutableFlag(..)
  , ObjectField(..)
  , OpenDescription(..)
  , PackageType
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
  , WithConstraint(..)
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
  | PtypObject [ObjectField] ClosedFlag
  | PtypClass (Loc Longident) [CoreType]
  | PtypAlias CoreType String
  | PtypVariant [RowField] ClosedFlag (Maybe [Label])
  | PtypPoly [Loc String] CoreType
  | PtypPackage PackageType
  | PtypExtension Extension
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
  | PexpVariant Label (Maybe Expression)
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
  | PexpSend Expression (Loc Label)
  | PexpNew (Loc Longident)
  | PexpSetInstVar (Loc Label) Expression
  | PexpOverride [(Loc Label, Expression)]
  | PexpLetModule (Loc String) ModuleExpr Expression
  | PexpLetException ExtensionConstructor (Loc Expression)
  | PexpAssert Expression
  | PexpLazy Expression
  | PexpPoly Expression (Maybe CoreType)
  | PexpObject ClassStructure
  | PexpNewType (Loc String) Expression
  | PexpPack ModuleExpr
  | PexpOpen OverrideFlag (Loc Longident) Expression
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
  | PpatInterval Constant Constant
  | PpatTuple [Pattern]
  | PpatConstruct (Loc Longident) (Maybe Pattern)
  | PpatVariant Label (Maybe Pattern)
  | PpatRecord [(Loc Longident, Pattern)] ClosedFlag
  | PpatArray [Pattern]
  | PpatOr Pattern Pattern
  | PpatConstraint Pattern CoreType
  | PpatType (Loc Longident)
  | PpatLazy Pattern
  | PpatUnpack (Loc String)
  | PpatException Pattern
  | PpatExtension Extension
  | PpatOpen (Loc Longident) Pattern
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
  | PmodStructure Structure
  | PmodFunctor (Loc String) (Maybe ModuleType) ModuleExpr
  | PmodApply ModuleExpr ModuleExpr
  | PmodConstraint ModuleExpr ModuleType
  | PmodUnpack Expression
  | PmodExtension Extension
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
  = Rtag (Loc Label) Attributes Bool [CoreType]
  | Rinherit CoreType
  deriving (Eq, Generic, Show)

data ClassStructure = ClassStructure
  { pcstrSelf   :: Pattern
  , pcstrFields :: [ClassField]
  }
  deriving (Eq, Generic, Show)

data ClassField = ClassField
  { pcfDesc       :: ClassFieldDesc
  , pcfLoc        :: Location
  , pcfAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ClassFieldDesc
  = PcfInherit OverrideFlag ClassExpr (Maybe (Loc String))
  | PcfVal (Loc Label) MutableFlag ClassFieldKind
  | PcfMethod (Loc Label) PrivateFlag ClassFieldKind
  | PcfConstraint CoreType CoreType
  | PcfInitializer Expression
  | PcfAttribute Attribute
  | PcfExtension Extension
  deriving (Eq, Generic, Show)

data ClassExpr = ClassExpr
  { pclDesc       :: ClassExprDesc
  , pclLoc        :: Location
  , pclAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ClassExprDesc
  = PclConstr (Loc Longident) [CoreType]
  | PclStructure ClassStructure
  | PclFun ArgLabel (Maybe Expression) Pattern ClassExpr
  | PclApply ClassExpr [(ArgLabel, Expression)]
  | PclLet RecFlag [ValueBinding] ClassExpr
  | PclConstraint ClassExpr ClassType
  | PclExtension Extension
  | PclOpen OverrideFlag (Loc Longident) ClassExpr
  deriving (Eq, Generic, Show)

data ClassType = ClassType
  { pctyDesc       :: ClassTypeDesc
  , pctyLoc        :: Location
  , pctyAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ClassTypeDesc
  = PctyConstr (Loc Longident) [CoreType]
  | PctySignature ClassSignature
  | PctyArrow ArgLabel CoreType ClassType
  | PctyExtension Extension
  | PctyOpen OverrideFlag (Loc Longident) ClassType
  deriving (Eq, Generic, Show)

data ClassSignature = ClassSignature
  { pcsigSelf :: CoreType
  , pcsigFields :: [ClassTypeField]
  }
  deriving (Eq, Generic, Show)

data ClassTypeField = ClassTypeField
  { pctfDesc       :: ClassTypeFieldDesc
  , pctfLoc        :: Location
  , pctfAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ClassTypeFieldDesc
  = PctfInherit ClassType
  | PctfVal (Loc Label) MutableFlag VirtualFlag CoreType
  | PtfMethod (Loc Label) PrivateFlag VirtualFlag CoreType
  | PctfConstraint CoreType CoreType
  | PctfAttribute Attribute
  | PctfExtension Extension
  deriving (Eq, Generic, Show)

data ClassFieldKind
  = CfkVirtual CoreType
  | CfkConcrete OverrideFlag Expression
  deriving (Eq, Generic, Show)

data ObjectField
  = Otag (Loc Label) Attributes CoreType
  | Oinherit CoreType
  deriving (Eq, Generic, Show)

data ModuleType = ModuleType
  { pmtyDesc       :: ModuleTypeDesc
  , pmtyLoc        :: Location
  , pmtyAttributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data ModuleTypeDesc
  = PmtyIdent (Loc Longident)
  | PmtySignature Signature
  | PmtyFunctor (Loc String) (Maybe ModuleType) ModuleType
  | PmtyWith ModuleType [WithConstraint]
  | PmtyTypeOf ModuleExpr
  | PmtyExtension Extension
  | PmtyAlias (Loc Longident)
  deriving (Eq, Generic, Show)

data WithConstraint
  = PwithType (Loc Longident) TypeDeclaration
  | PwithModule (Loc Longident) (Loc Longident)
  | PwithTypeSubst (Loc Longident) TypeDeclaration
  | PwithModSubst (Loc Longident) (Loc Longident)
  deriving (Eq, Generic, Show)

type PackageType = (Loc Longident, [(Loc Longident, CoreType)])
