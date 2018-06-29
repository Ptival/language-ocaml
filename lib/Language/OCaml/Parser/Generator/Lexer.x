{

{-# OPTIONS_GHC -w #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Parser.Generator.Lexer
  ( Alex(..)
  , Located(..)
  , SrcSpan(..)
  , Token(..)
  , alexError
  , alexScanTokens
  , lexWrap
  , runAlex
  ) where

import qualified Data.Map.Strict as M
import           GHC.Generics
import           Text.Printf

}

%wrapper "monad"

-- Character set macros
$blank     = [' ' \t \012]
$lowercase = [a-z _]
$uppercase = [A-Z]
$identchar = [A-Z a-z _ '\'' 0-9]

-- Regular expression macros
@newline = \r*\n

tokens :-
  -- TODO: \\ newline
  -- TODO: newline
  -- TODO: blank +
  '_'                    { tok  TokUnderscore }
  '~'                    { tok  TokTilde }
  -- TODO: LABEL
  -- TODO: QUESTION
  -- TODO: OPTLABEL
  $lowercase $identchar* { keywordLIdent }
  $uppercase $identchar* { tokS TokUIdent }
  -- TODO
  '&'    { tok TokAmpersand }
  "&&"   { tok TokAmperAmper }
  '`'    { tok TokBackQuote }
  '\''   { tok TokQuote }
  '\('   { tok TokLParen }
  '\)'   { tok TokRParen }
  '*'    { tok TokStar }
  '\,'   { tok TokComma }
  "->"   { tok TokMinusGreater }
  '.'    { tok TokDot }
  ".."   { tok TokDotDot }
  -- TODO
  ':'    { tok TokColon }
  "::"   { tok TokColonColon }
  ":="   { tok TokColonEqual }
  ":>"   { tok TokColonGreater }
  ';'    { tok TokSemi }
  ";;"   { tok TokSemiSemi }
  '\<'   { tok TokLess }
  "<-"   { tok TokLessMinus }
  '='    { tok TokEqual }
  '\['   { tok TokLBracket }
  "[|"   { tok TokLBracketBar }
  "[<"   { tok TokLBracketLess }
  "[>"   { tok TokLBracketGreater }
  '\]'   { tok TokRBracket }
  '\{'   { tok TokLBrace }
  "{<"   { tok TokLBraceLess }
  '|'    { tok TokBar }
  "||"   { tok TokBarBar }
  "|]"   { tok TokBarRBracket }
  '>'    { tok TokGreater }
  ">]"   { tok TokGreaterRBracket }
  '\}'   { tok TokRBrace }
  ">}"   { tok TokGreaterRBrace }
  "[@"   { tok TokLBracketAt }
  "[@@"  { tok TokLBracketAtAt }
  "[@@@" { tok TokLBracketAtAtAt }
  "[%"   { tok TokLBracketPercent }
  "[%%"  { tok TokLBracketPercentPercent }
  '!'    { tok TokBang }
  -- TODO

{

data Located a = Located SrcSpan a
  deriving (Generic, Show)

data Token
  = TokAmperAmper
  | TokAmpersand
  | TokAnd
  | TokAs
  | TokAssert
  | TokBackQuote
  | TokBang
  | TokBar
  | TokBarBar
  | TokBarRBracket
  | TokBegin
  | TokClass
  | TokColon
  | TokColonColon
  | TokColonEqual
  | TokColonGreater
  | TokComma
  | TokConstraint
  | TokDo
  | TokDone
  | TokDot
  | TokDotDot
  | TokDownTo
  | TokElse
  | TokEnd
  | TokEOF
  | TokEqual
  | TokException
  | TokExternal
  | TokFalse
  | TokFor
  | TokFun
  | TokFunction
  | TokFunctor
  | TokGreater
  | TokGreaterRBrace
  | TokGreaterRBracket
  | TokIf
  | TokIn
  | TokInclude
  | TokInherit
  | TokInitializer
  | TokLazy
  | TokLBrace
  | TokLBraceLess
  | TokLBracket
  | TokLBracketAt
  | TokLBracketAtAt
  | TokLBracketAtAtAt
  | TokLBracketBar
  | TokLBracketGreater
  | TokLBracketLess
  | TokLBracketPercent
  | TokLBracketPercentPercent
  | TokLess
  | TokLessMinus
  | TokLet
  | TokLIdent String
  | TokLParen
  | TokMatch
  | TokMethod
  | TokMinus
  | TokMinusGreater
  | TokModule
  | TokMutable
  | TokNew
  | TokNonRec
  | TokObject
  | TokOf
  | TokOpen
  | TokOr
  | TokPercent
  | TokPrivate
  | TokQuote
  | TokRBrace
  | TokRBracket
  | TokRec
  | TokRParen
  | TokSemi
  | TokSemiSemi
  | TokSig
  | TokStar
  | TokStruct
  | TokThen
  | TokTilde
  | TokTo
  | TokTrue
  | TokTry
  | TokType
  | TokUIdent String
  | TokUnderscore
  | TokVal
  | TokVirtual
  | TokWhen
  | TokWhile
  | TokWith
  deriving (Generic, Show)

data SrcSpan = SrcSpan
  { srcSpanStartLine :: !Int
  , srcSpanStartCol  :: !Int
  , srcSpanEndLine   :: !Int
  , srcSpanEndCol    :: !Int
  }
  deriving (Generic, Eq, Ord)

instance Show SrcSpan where
  show SrcSpan {..} =
    printf "(%d,%d)-(%d,%d)" srcSpanStartLine srcSpanStartCol srcSpanEndLine srcSpanEndCol

alexEOF :: Alex (Located Token)
alexEOF = do
  (l, c) <- getPosition
  let ss = SrcSpan l c l c
  return $ Located ss TokEOF

alexScanTokens :: String -> Either String [Located Token]
alexScanTokens input = runAlex input gather
  where
    gather = do
      t <- alexMonadScan
      case t of
        Located _ TokEOF -> return [t]
        _                -> return . (t :) =<< gather

getPos :: AlexPosn -> (Int, Int)
-- NOTE: OCaml starts at column 0, but Alex starts at column 1
getPos (AlexPn _ line column) = (line, column - 1)

getPosition :: Alex (Int, Int)
getPosition = Alex $ \ s -> Right (s, getPos . alex_pos $ s)

keywordLIdent :: AlexInput -> Int -> Alex (Located Token)
keywordLIdent = tokS (\ s -> M.findWithDefault (TokLIdent s) s keywordTable)

keywordTable :: M.Map String Token
keywordTable = M.fromList
  [ ("and",         TokAnd)
  , ("as",          TokAs)
  , ("assert",      TokAssert)
  , ("begin",       TokBegin)
  , ("class",       TokClass)
  , ("constraint",  TokConstraint)
  , ("do",          TokDo)
  , ("done",        TokDone)
  , ("downto",      TokDownTo)
  , ("else",        TokElse)
  , ("end",         TokEnd)
  , ("exception",   TokException)
  , ("external",    TokExternal)
  , ("false",       TokFalse)
  , ("for",         TokFor)
  , ("fun",         TokFun)
  , ("function",    TokFunction)
  , ("functor",     TokFunctor)
  , ("if",          TokIf)
  , ("in",          TokIn)
  , ("include",     TokInclude)
  , ("inherit",     TokInherit)
  , ("initializer", TokInitializer)
  , ("lazy",        TokLazy)
  , ("let",         TokLet)
  , ("match",       TokMatch)
  , ("method",      TokMethod)
  , ("module",      TokModule)
  , ("mutable",     TokMutable)
  , ("new",         TokNew)
  , ("nonrec",      TokNonRec)
  , ("object",      TokObject)
  , ("of",          TokOf)
  , ("open",        TokOpen)
  , ("or",          TokOr)
  , ("private",     TokPrivate)
  , ("rec",         TokRec)
  , ("sig",         TokSig)
  , ("struct",      TokStruct)
  , ("then",        TokThen)
  , ("to",          TokTo)
  , ("true",        TokTrue)
  , ("try",         TokTry)
  , ("type",        TokType)
  , ("val",         TokVal)
  , ("virtual",     TokVirtual)
  , ("when",        TokWhen)
  , ("while",       TokWhile)
  , ("with",        TokWith)
  ]

lexWrap :: (Located Token -> Alex a) -> Alex a
lexWrap cont = alexMonadScan >>= cont

tok :: Token -> AlexInput -> Int -> Alex (Located Token)
tok t (p, _, _, _) len = do
  let (l, c) = getPos p
  let ss = SrcSpan l c l (c + len)
  return $ Located ss t

tokS :: (String -> Token) -> AlexInput -> Int -> Alex (Located Token)
tokS t input@(_, _, _, str) len = tok (t $ take len str) input len

}
