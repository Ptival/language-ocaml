{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Docstrings
  ( Docs
  , Docstring(..)
  , Info
  , Text
  , add_docs_attrs
  , add_info_attrs
  , add_text_attrs
  , docs_attr
  , empty_docs
  , empty_info
  , rhs_text
  , text_attr
  ) where

import GHC.Generics
import Prelude hiding (exp)

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.StdLib.Parsing

data Ds_attached
   = Unattached   {- Not yet attached anything. -}
   | Info         {- Attached to a field or constructor. -}
   | Docs         {- Attached to an item or as floating text. -}
  deriving (Eq, Generic, Show)

{- A docstring is "associated" with an item if there are no blank lines between
   them. This is used for generating docstring ambiguity warnings. -}
data Ds_associated
  = Zero             {- Not associated with an item -}
  | One              {- Associated with one item -}
  | Many
  deriving (Eq, Generic, Show)

data Docstring = Docstring
  { ds_body       :: String
  , ds_loc        :: Location
  , ds_attached   :: Ds_attached
  , ds_associated :: Ds_associated
  }
  deriving (Eq, Generic, Show)

type Text = [Docstring]

text_loc :: Loc String
text_loc = Loc
  { txt = "ocaml.text"
  , loc = none
  }

text_attr :: Docstring -> Attribute
text_attr ds =
  let exp =
        Expression
        { pexp_desc       = Pexp_constant (Pconst_string (ds_body ds) Nothing)
        , pexp_loc        = ds_loc ds
        , pexp_attributes = []
        }
  in
  let item =
        Structure_item
        { pstr_desc = Pstr_eval exp []
        , pstr_loc  = pexp_loc exp
        }
  in
  (text_loc, PStr [item])

add_text_attrs :: [Docstring] -> [Attribute] -> [Attribute]
add_text_attrs dsl attrs = (map text_attr dsl) ++ attrs

rhs_text :: a -> [b]
rhs_text pos = get_text (rhs_start_pos pos)

get_text :: a -> [b]
get_text _pos = [] -- FIXME

data Docs = Docs'
  { docs_pre  :: Maybe Docstring
  , docs_post :: Maybe Docstring
  }
  deriving (Eq, Generic, Show)

add_docs_attrs :: Docs -> [(Loc String, Payload)] -> [(Loc String, Payload)]
add_docs_attrs docs attrs =
  let attrs1 = case docs_pre docs of
               Nothing -> attrs
               Just ds -> docs_attr ds : attrs
  in
  let attrs2 = case docs_post docs of
               Nothing -> attrs1
               Just ds -> attrs1 ++ [docs_attr ds]
  in
  attrs2

add_info_attrs :: a -> b -> b
add_info_attrs _info attrs = attrs -- FIXME

empty_docs :: Docs
empty_docs =
  Docs'
  { docs_pre = Nothing
  , docs_post = Nothing
  }

doc_loc :: Loc String
doc_loc =
  Loc
  { txt = "ocaml.doc"
  , loc = none
  }

docs_attr :: Docstring -> (Loc String, Payload)
docs_attr ds =
  let exp = Expression
        { pexp_desc = Pexp_constant $ Pconst_string (ds_body ds) Nothing
        , pexp_loc = ds_loc ds
        , pexp_attributes = []
        }
  in
  let item = Structure_item
        { pstr_desc = Pstr_eval exp []
        , pstr_loc = pexp_loc exp
        }
  in
  (doc_loc, PStr [item])

type Info = Maybe Docstring

empty_info :: Info
empty_info = Nothing
