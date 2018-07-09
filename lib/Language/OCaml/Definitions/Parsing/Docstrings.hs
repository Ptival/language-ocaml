{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.Docstrings
  ( Docs
  , Docstring(..)
  , Info
  , Text
  , addDocsAttrs
  , addInfoAttrs
  , addTextAttrs
  , docsAttr
  , docstringBody
  , docstringLoc
  , emptyDocs
  , emptyInfo
  , rhsPostText
  , rhsPreText
  , rhsPostExtraText
  , rhsPreExtraText
  , rhsText
  , symbolInfo
  , symbolText
  , symbolTextLazy
  , textAttr
  ) where

import           GHC.Generics
import qualified Data.Map.Strict as M
import           Prelude hiding (exp)

import           Language.OCaml.Definitions.StdLib.Lexing
import           Language.OCaml.Definitions.StdLib.Parsing as Parsing
import           Language.OCaml.Definitions.Parsing.ASTTypes
import           Language.OCaml.Definitions.Parsing.Location
import           Language.OCaml.Definitions.Parsing.ParseTree

data DsAttached
   = Unattached   {- Not yet attached anything. -}
   | Info         {- Attached to a field or constructor. -}
   | Docs         {- Attached to an item or as floating text. -}
  deriving (Eq, Generic, Show)

{- A docstring is "associated" with an item if there are no blank lines between
   them. This is used for generating docstring ambiguity warnings. -}
data DsAssociated
  = Zero             {- Not associated with an item -}
  | One              {- Associated with one item -}
  | Many
  deriving (Eq, Generic, Show)

data Docstring = Docstring
  { dsBody       :: String
  , dsLoc        :: Location
  , dsAttached   :: DsAttached
  , dsAssociated :: DsAssociated
  }
  deriving (Eq, Generic, Show)

type Text = [Docstring]

textLoc :: Loc String
textLoc = Loc
  { txt = "ocaml.text"
  , loc = none
  }

textAttr :: Docstring -> Attribute
textAttr ds =
  let exp =
        Expression
        { pexpDesc       = PexpConstant (PconstString (dsBody ds) Nothing)
        , pexpLoc        = dsLoc ds
        , pexpAttributes = []
        }
  in
  let item =
        StructureItem
        { pstrDesc = PstrEval exp []
        , pstrLoc  = pexpLoc exp
        }
  in
  (textLoc, PStr [item])

addTextAttrs :: [Docstring] -> [Attribute] -> [Attribute]
addTextAttrs dsl attrs = (map textAttr dsl) ++ attrs

rhsText :: a -> [b]
rhsText pos = getText (rhsStartPos pos)

getText :: a -> [b]
getText _pos = [] -- FIXME

data Docs = Docs'
  { docsPre  :: Maybe Docstring
  , docsPost :: Maybe Docstring
  }
  deriving (Eq, Generic, Show)

addDocsAttrs :: Docs -> [(Loc String, Payload)] -> [(Loc String, Payload)]
addDocsAttrs docs attrs =
  let attrs1 = case docsPre docs of
               Nothing -> attrs
               Just ds -> docsAttr ds : attrs
  in
  let attrs2 = case docsPost docs of
               Nothing -> attrs1
               Just ds -> attrs1 ++ [docsAttr ds]
  in
  attrs2

addInfoAttrs :: a -> b -> b
addInfoAttrs _info attrs = attrs -- FIXME

emptyDocs :: Docs
emptyDocs =
  Docs'
  { docsPre = Nothing
  , docsPost = Nothing
  }

docLoc :: Loc String
docLoc =
  Loc
  { txt = "ocaml.doc"
  , loc = none
  }

docsAttr :: Docstring -> (Loc String, Payload)
docsAttr ds =
  let exp = Expression
        { pexpDesc = PexpConstant $ PconstString (dsBody ds) Nothing
        , pexpLoc = dsLoc ds
        , pexpAttributes = []
        }
  in
  let item = StructureItem
        { pstrDesc = PstrEval exp []
        , pstrLoc = pexpLoc exp
        }
  in
  (docLoc, PStr [item])

type Info = Maybe Docstring

emptyInfo :: Info
emptyInfo = Nothing

docstringBody :: Docstring -> String
docstringBody = dsBody

docstringLoc :: Docstring -> Location
docstringLoc = dsLoc

symbolInfo :: () -> Info
symbolInfo () = Nothing -- FIXME

symbolText :: () -> Text
symbolText () = [] -- FIXM

symbolTextLazy :: () -> Text
symbolTextLazy () = [] -- FIXMEE

preTable :: M.Map Position Text
preTable = M.empty

postTable :: M.Map Position Text
postTable = M.empty

preExtraTable :: M.Map Position Text
preExtraTable = M.empty

postExtraTable :: M.Map Position Text
postExtraTable = M.empty

getDocstrings :: [Docstring] -> [Docstring]
getDocstrings dsl = loop [] dsl
  where
    loop acc = \case
      []                                           -> reverse acc
      (Docstring {..}) : rest | dsAttached == Info -> loop acc rest
      ds : rest                                    -> loop (ds { dsAttached = Docs } : acc) rest

getPreText :: Position -> Text
getPreText pos = getDocstrings $ M.findWithDefault [] pos preTable

rhsPreText :: a -> Text
rhsPreText pos = getPreText (Parsing.rhsStartPos pos)

getPostText :: Position -> Text
getPostText pos = getDocstrings $ M.findWithDefault [] pos postTable

rhsPostText :: a -> Text
rhsPostText pos = getPostText (Parsing.rhsEndPos pos)

getPreExtraText :: Position -> Text
getPreExtraText pos = getDocstrings $ M.findWithDefault [] pos preExtraTable

rhsPreExtraText :: a -> Text
rhsPreExtraText pos = getPreExtraText (Parsing.rhsStartPos pos)

getPostExtraText :: Position -> Text
getPostExtraText pos = getDocstrings $ M.findWithDefault [] pos postExtraTable

rhsPostExtraText :: a -> Text
rhsPostExtraText pos = getPostExtraText (Parsing.rhsEndPos pos)
