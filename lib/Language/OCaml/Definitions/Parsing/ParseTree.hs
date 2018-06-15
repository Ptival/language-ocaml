{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute
  , Attributes
  , Case(..)
  , Constant(..)
  , Constructor_arguments(..)
  , Constructor_declaration(..)
  , Core_type(..)
  , Core_type_desc(..)
  , Expression(..)
  , Expression_desc(..)
  , Extension
  , ExtensionConstructor(..)
  , ExtensionConstructorKind(..)
  , Label_declaration(..)
  , Longident(..)
  , Module_binding(..)
  , Module_expr(..)
  , Module_expr_desc(..)
  , Mutable_flag(..)
  , Open_description(..)
  , Pattern(..)
  , Pattern_desc(..)
  , Payload(..)
  , Private_flag(..)
  , Signature
  , Signature_item(..)
  , Signature_item_desc(..)
  , Structure
  , Structure_item(..)
  , Structure_item_desc(..)
  , Type_declaration(..)
  , TypeException(..)
  , TypeExtension(..)
  , Type_kind(..)
  , Value_binding(..)
  , Value_description(..)
  , field
  , none
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.ASTTypes hiding (Constant)
import Language.OCaml.Definitions.Parsing.Location

data Constant
  = Pconst_integer String (Maybe Char)
  | Pconst_char Char
  | Pconst_string String (Maybe String)
  | Pconst_float String (Maybe Char)
  deriving (Eq, Generic, Show)

type Attribute = (Loc String, Payload)
type Extension = (Loc String, Payload)
type Attributes = [Attribute]

data Payload
  = PStr Structure
  | PSig Signature
  | PTyp Core_type
  | PPat Pattern (Maybe Expression)
  deriving (Eq, Generic, Show)

data Core_type = Core_type
  { ptyp_desc       :: Core_type_desc
  , ptyp_loc        :: Location
  , ptyp_attributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data Core_type_desc
  = Ptyp_any
  | Ptyp_var String
  | Ptyp_arrow Arg_label Core_type Core_type
  | Ptyp_tuple [Core_type]
  | Ptyp_constr (Loc Longident) [Core_type]
  -- | Ptyp_object of object_field list * Asttypes.closed_flag
  | Ptyp_class (Loc Longident) [Core_type]
  | Ptyp_alias Core_type String
  -- | Ptyp_variant of row_field list * Asttypes.closed_flag * Asttypes.label list option
  | Ptyp_poly [Loc String] Core_type
  -- | Ptyp_package of package_type
  -- | Ptyp_extension of extension
  deriving (Eq, Generic, Show)

data Constructor_arguments
  = Pcstr_tuple [Core_type]
  -- | Pcstr_record [Label_declaration]
  deriving (Eq, Generic, Show)

data Private_flag
  = Private
  | Public
  deriving (Eq, Generic, Show)

data Type_declaration = Type_declaration
  { ptype_name :: Loc String
  , ptype_params :: [(Core_type, Variance)]
  , ptype_cstrs :: [(Core_type, Core_type, Location)]
  , ptype_kind :: Type_kind
  , ptype_private :: Private_flag
  , ptype_manifest :: Maybe Core_type
  , ptype_attributes :: Attributes
  , ptype_loc :: Location
  }
  deriving (Eq, Generic, Show)

data Type_kind
  = Ptype_abstract
  | Ptype_variant [Constructor_declaration]
  | Ptype_record [Label_declaration]
  | Ptype_open
  deriving (Eq, Generic, Show)

data Label_declaration = Label_declaration
  { pld_name :: Loc String
  , pld_mutable :: Mutable_flag
  , pld_type :: Core_type
  -- , pld_loc :: Location.t
  -- , pld_attributes :: attributes
  }
  deriving (Eq, Generic, Show)

data Constructor_declaration = Constructor_declaration
  { pcd_name       :: Loc String
  , pcd_args       :: Constructor_arguments
  , pcd_res        :: Maybe Core_type
  , pcd_loc        :: Location
  , pcd_attributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data Mutable_flag
  = Immutable
  | Mutable
  deriving (Eq, Generic, Show)

data Longident
  = Lident String
  | Ldot Longident String
  | Lapply Longident Longident
  deriving (Eq, Generic, Show)

field ::
  Mutable_flag ->
  Loc String ->
  Core_type ->
  Label_declaration
field {- loc attrs info -} mut name typ =
  Label_declaration
  { pld_name    = name
  , pld_mutable = mut
  , pld_type    = typ
  --, pld_loc :: Location.t
  --, pld_attributes :: attributes
  }

type Structure = [Structure_item]

data Structure_item = Structure_item
  { pstr_desc :: Structure_item_desc
  , pstr_loc  :: Location
  }
  deriving (Eq, Generic, Show)

data Structure_item_desc
  = Pstr_eval Expression Attributes
  | Pstr_value Rec_flag [Value_binding]
  -- | Pstr_primitive value_description
  | Pstr_type Rec_flag [Type_declaration]
  -- | Pstr_typext type_extension
  | Pstr_exception TypeException
  | Pstr_module Module_binding
  -- | Pstr_recmodule module_binding list
  -- | Pstr_modtype module_type_declaration
  | Pstr_open Open_description
  -- | Pstr_class class_declaration list
  -- | Pstr_class_type class_type_declaration list
  -- | Pstr_include include_declaration
  | Pstr_attribute Attribute
  | Pstr_extension Extension Attributes
  deriving (Eq, Generic, Show)

data Expression = Expression
  { pexp_desc       :: Expression_desc
  , pexp_loc        :: Location
  , pexp_attributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data Expression_desc
  = Pexp_ident (Loc Longident)
  | Pexp_constant Constant
  | Pexp_let Rec_flag [Value_binding] Expression
  | Pexp_function [Case]
  | Pexp_fun Arg_label (Maybe Expression) Pattern Expression
  | Pexp_apply Expression [(Arg_label, Expression)]
  | Pexp_match Expression [Case]
  -- | Pexp_try expression * case list
  | Pexp_tuple [Expression]
  | Pexp_construct (Loc Longident) (Maybe Expression)
  -- | Pexp_variant Asttypes.label * expression option
  -- | Pexp_record (Longident.t Asttypes.loc * expression) list * expression option
  | Pexp_field Expression (Loc Longident)
  -- | Pexp_setfield expression * Longident.t Asttypes.loc * expression
  -- | Pexp_array expression list
  | Pexp_ifthenelse Expression Expression (Maybe Expression)
  | Pexp_sequence Expression Expression
  -- | Pexp_while expression * expression
  -- | Pexp_for pattern * expression * expression * Asttypes.direction_flag * expression
  | Pexp_constraint Expression Core_type
  | Pexp_coerce Expression (Maybe Core_type) Core_type
  -- | Pexp_send expression * Asttypes.label Asttypes.loc
  -- | Pexp_new Longident.t Asttypes.loc
  -- | Pexp_setinstvar Asttypes.label Asttypes.loc * expression
  -- | Pexp_override (Asttypes.label Asttypes.loc * expression) list
  -- | Pexp_letmodule string Asttypes.loc * module_expr * expression
  -- | Pexp_letexception extension_constructor * expression
  -- | Pexp_assert expression
  -- | Pexp_lazy expression
  -- | Pexp_poly expression * core_type option
  -- | Pexp_object class_structure
  -- | Pexp_newtype string Asttypes.loc * expression
  -- | Pexp_pack module_expr
  -- | Pexp_open Asttypes.override_flag * Longident.t Asttypes.loc * expression
  | Pexp_extension Extension
  -- | Pexp_unreachable
  deriving (Eq, Generic, Show)

type Signature = [Signature_item]

data Signature_item = Signature_item
  { psig_desc :: Signature_item_desc
  , psig_loc  :: Location
  }
  deriving (Eq, Generic, Show)

data Signature_item_desc
  = Psig_value Value_description
  -- | Psig_type Asttypes.rec_flag * type_declaration list
  -- | Psig_typext type_extension
  -- | Psig_exception extension_constructor
  -- | Psig_module module_declaration
  -- | Psig_recmodule module_declaration list
  -- | Psig_modtype module_type_declaration
  -- | Psig_open open_description
  -- | Psig_include include_description
  -- | Psig_class class_description list
  -- | Psig_class_type class_type_declaration list
  -- | Psig_attribute attribute
  -- | Psig_extension extension * attributes
  deriving (Eq, Generic, Show)

data Value_description = Value_description
  { pval_name       :: Loc String
  , pval_type       :: Core_type
  , pval_prim       :: [String]
  , pval_attributes :: Attributes
  , pval_loc        :: Location
  }
  deriving (Eq, Generic, Show)

data Pattern = Pattern
  { ppat_desc       :: Pattern_desc
  , ppat_loc        :: Location
  , ppat_attributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data Pattern_desc
  = Ppat_any
  | Ppat_var (Loc String)
  | Ppat_alias Pattern (Loc String)
  | Ppat_constant Constant
  -- | Ppat_interval constant * constant
  | Ppat_tuple [Pattern]
  | Ppat_construct (Loc Longident) (Maybe Pattern)
  -- | Ppat_variant Asttypes.label * pattern option
  -- | Ppat_record (Longident.t Asttypes.loc * pattern) list * Asttypes.closed_flag
  -- | Ppat_array pattern list
  | Ppat_or Pattern Pattern
  | Ppat_constraint Pattern Core_type
  -- | Ppat_type Longident.t Asttypes.loc
  -- | Ppat_lazy pattern
  -- | Ppat_unpack string Asttypes.loc
  -- | Ppat_exception pattern
  -- | Ppat_extension extension
  -- | Ppat_open Longident.t Asttypes.loc * pattern
  deriving (Eq, Generic, Show)

data Open_description = Open_description
  { popen_lid        :: Loc Longident
  , popen_override   :: Override_flag
  , popen_loc        :: Location
  , popen_attributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data Module_expr = Module_expr
  { pmod_desc       :: Module_expr_desc
  , pmod_loc        :: Location
  , pmod_attributes :: Attributes
  }
  deriving (Eq, Generic, Show)

data Module_expr_desc
  = Pmod_ident (Loc Longident)
  -- | Pmod_structure of structure
  -- | Pmod_functor of string Asttypes.loc * module_type option * module_expr
  -- | Pmod_apply of module_expr * module_expr
  -- | Pmod_constraint of module_expr * module_type
  -- | Pmod_unpack of expression
  -- | Pmod_extension of extension
  deriving (Eq, Generic, Show)

data Module_binding = Module_binding
  { pmb_name       :: Loc String
  , pmb_expr       :: Module_expr
  , pmb_attributes :: Attributes
  , pmb_loc        :: Location
  }
  deriving (Eq, Generic, Show)

data Value_binding = Value_binding
  { pvb_pat        :: Pattern
  , pvb_expr       :: Expression
  , pvb_attributes :: Attributes
  , pvb_loc        :: Location
  }
  deriving (Eq, Generic, Show)

data Case = Case
  { pc_lhs   :: Pattern
  , pc_guard :: Maybe Expression
  , pc_rhs   :: Expression
  }
  deriving (Eq, Generic, Show)

data ExtensionConstructor = ExtensionConstructor
  { pext_name        :: Loc String
  , pext_kind        :: ExtensionConstructorKind
  , pext_loc         :: Location
  , pext_attributes  :: Attributes -- C of ... [@id1] [@id2]
  }
  deriving (Eq, Generic, Show)

data ExtensionConstructorKind
  = Pext_decl Constructor_arguments (Maybe Core_type)
  | Pext_rebind (Loc Longident)
  deriving (Eq, Generic, Show)

data TypeExtension = TypeExtension
  { ptyext_path         :: Loc Longident
  , ptyext_params       :: [(Core_type, Variance)]
  , ptyext_constructors :: [ExtensionConstructor]
  , ptyext_private      :: Private_flag
  , ptyext_attributes   :: Attributes
  }
  deriving (Eq, Generic, Show)

data TypeException = TypeException
  { ptyexn_constructor :: ExtensionConstructor
  , ptyexn_attributes  :: Attributes
  }
  deriving (Eq, Generic, Show)
