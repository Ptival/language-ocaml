{

{-# OPTIONS_GHC -w #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Language.OCaml.Parser.Generator.Lexer
  ( Alex(..)
  , IntermediateToken
  , Located(..)
  , ResultToken
  , SrcSpan(..)
  , Token(..)
  , alexError
  , alexScanTokens
  , lexWrap
  , runAlex
  ) where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.State
import qualified Data.Map.Strict as M
import           GHC.Generics
import           Text.Printf

}

%wrapper "monadUserState"

-- Character set macros
$blank      = [\  \t \f]
$identChar  = [A-Z a-z _ '\'' 0-9]
$lowerCase  = [a-z _]
$symbolChar = [!]--  '$' '%']--  '&' '*' '+' '-' '.' ] -- '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
$upperCase  = [A-Z]

-- Regular expression macros
@binLiteral      = 0 [bB] [01] [01_]*
@decimalLiteral  = [0-9] [0-9 _]*
@floatLiteral    = [0-9] [0-9 _]* (\. [0-9 _]*)? ([eE] [\+\-]? [0-9] [0-9 _]*)?
@hexFloatLiteral = 0 [xX] [0-9 A-F a-f] [0-9 A-F a-f _]* (\. [0-9 A-F a-f _]*)? ([pP] [\+\-]? [0-9] [0-9 _]*)?
@hexLiteral      = 0 [xX] [0-9 A-F a-f] [0-9 A-F a-f _]
@literalModifier = [G-Z g-z]
@newLine         = \r* \n
@octLiteral      = 0 [oO] [0-7] [0-7 _]*
-- must be defined in order, so these must go last
@intLiteral      = @decimalLiteral | @hexLiteral | @octLiteral | @binLiteral

tokens :-
  -- TODO: \\ newline
  <0> @newLine;
  <0> $blank+;
  -- TODO: blank +
  <0> "_"                                                 { tok  TokUnderscore }
  <0> "~"                                                 { tok  TokTilde }
  -- TODO: LABEL
  -- TODO: QUESTION
  -- TODO: OPTLABEL
  <0> $lowerCase $identChar*                              { keywordLIdent }
  <0> $upperCase $identChar*                              { tokS TokUIdent }
  <0> @intLiteral                                         { tokS $ \s -> TokInt (s, Nothing) }
  <0> @intLiteral @literalModifier                        { tokS $
                                                            \ s ->
                                                            let (lit, mod) = extractModifier s in
                                                            TokInt (lit, (Just mod))
                                                          }
  <0> @floatLiteral | @hexFloatLiteral                    { tokS $ \ s -> TokFloat (s, Nothing) }
  <0> (@floatLiteral | @hexFloatLiteral) @literalModifier { tokS $
                                                            \ s ->
                                                            let (lit, mod) = extractModifier s in
                                                            TokFloat (lit, (Just mod))
                                                          }
  -- TODO
  <0> "&"    { tok TokAmpersand }
  <0> "&&"   { tok TokAmperAmper }
  <0> "`"    { tok TokBackQuote }
  <0> "'"    { tok TokQuote }
  <0> "("    { tok TokLParen }
  <0> ")"    { tok TokRParen }
  <0> "*"    { tok TokStar }
  <0> ","    { tok TokComma }
  <0> "->"   { tok TokMinusGreater }
  <0> "."    { tok TokDot }
  <0> ".."   { tok TokDotDot }
  -- TODO
  <0> ":"    { tok TokColon }
  <0> "::"   { tok TokColonColon }
  <0> ":="   { tok TokColonEqual }
  <0> ":>"   { tok TokColonGreater }
  <0> ";"    { tok TokSemi }
  <0> ";;"   { tok TokSemiSemi }
  <0> "<"    { tok TokLess }
  <0> "<-"   { tok TokLessMinus }
  <0> "="    { tok TokEqual }
  <0> "["    { tok TokLBracket }
  <0> "[|"   { tok TokLBracketBar }
  <0> "[<"   { tok TokLBracketLess }
  <0> "[>"   { tok TokLBracketGreater }
  <0> "]"    { tok TokRBracket }
  <0> "{"    { tok TokLBrace }
  <0> "{<"   { tok TokLBraceLess }
  <0> "|"    { tok TokBar }
  <0> "||"   { tok TokBarBar }
  <0> "|]"   { tok TokBarRBracket }
  <0> ">"    { tok TokGreater }
  <0> ">]"   { tok TokGreaterRBracket }
  <0> "}"    { tok TokRBrace }
  <0> ">}"   { tok TokGreaterRBrace }
  <0> "[@"   { tok TokLBracketAt }
  <0> "[@@"  { tok TokLBracketAtAt }
  <0> "[@@@" { tok TokLBracketAtAtAt }
  <0> "[%"   { tok TokLBracketPercent }
  <0> "[%%"  { tok TokLBracketPercentPercent }
  <0> "!"    { tok TokBang }
  -- TODO: !=
  <0> "+"    { tok TokPlus }
  <0> "+."   { tok TokPlusDot }
  <0> "+="   { tok TokPlusEq }
  <0> "-"    { tok TokMinus }
  <0> "-."   { tok TokMinusDot }
  -- TODO: pre/in fix ops
  <0> [= \< > \| & \$] $symbolChar* { tokS TokInfixOp0 }

  -- Now let's deal with strings and comments
  <0>    \"       { beginString }
  <s>    \"       { endString }
  <s>    .        { appendString }
  <s>    \\[nt\"] { escapeString }
  <0, c> "(*"     { beginComment }
  <c>    "*)"     { endComment }
  <c>    [.\n]    ;

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
  | TokChar Char
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
  | TokDotOp String
  | TokDownTo
  | TokElse
  | TokEnd
  | TokEOF
  | TokEqual
  | TokException
  | TokExternal
  | TokFalse
  | TokFloat (String, (Maybe Char))
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
  | TokInfixOp0 String
  | TokInfixOp1 String
  | TokInfixOp2 String
  | TokInfixOp3 String
  | TokInherit
  | TokInitializer
  | TokInt (String, (Maybe Char))
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
  | TokMinusDot
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
  | TokPlus
  | TokPlusDot
  | TokPlusEq
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
  | TokString (String, Maybe String)
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

type ResultToken       = Located Token
type IntermediateToken = Maybe ResultToken
type MyAlexAction      = AlexAction IntermediateToken

alexEOF :: Alex IntermediateToken
alexEOF = do
  (l, c) <- getPosition
  let ss = SrcSpan l c l c
  return $ Just $ Located ss TokEOF

myAlexMonadScan :: Alex ResultToken
myAlexMonadScan = alexMonadScan >>= \case
  Just t  -> return t
  Nothing -> myAlexMonadScan

lexWrap :: (ResultToken -> Alex a) -> Alex a
lexWrap cont = myAlexMonadScan >>= cont

alexScanTokens :: String -> Either String [ResultToken]
alexScanTokens input = runAlex input gather
  where
    gather :: Alex [ResultToken]
    gather = do
      myAlexMonadScan >>= \case
        t@(Located _ TokEOF) -> return [t]
        t                    -> return . (t :) =<< gather

getPos :: AlexPosn -> MyPosition
-- NOTE: OCaml starts at column 0, but Alex starts at column 1
getPos (AlexPn _ line col) = (adjustLine line, adjustCol col)

getPosition :: Alex MyPosition
getPosition = Alex $ \ s -> Right (s, getPos . alex_pos $ s)

keywordLIdent :: MyAlexAction
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

-- I like to use 'c' and 's' for the startCodes, but it's not very
-- documentation-friendly so here are some aliases

codeStartCode :: Int
codeStartCode = 0

commentStartCode :: Int
commentStartCode = c

stringStartCode :: Int
stringStartCode = s

tok :: Token -> MyAlexAction
tok t (p, _, _, _) len = do
  let (l, c) = getPos p
  let ss = SrcSpan l c l (c + len)
  return $ Just $ Located ss t

tokS :: (String -> Token) -> MyAlexAction
tokS t input@(_, _, _, str) len = tok (t $ take len str) input len

extractModifier :: String -> (String, Char)
extractModifier s = case reverse s of
  mod : lit -> (lit, mod)
  _         -> error "This should not happen"

getState :: Alex AlexState
getState = Alex $ \ s -> return (s, s)

getUserState :: Alex AlexUserState
getUserState = Alex $ \ s -> return (s, alex_ust s)

setUserState :: AlexUserState -> Alex ()
setUserState us = Alex $ \ s -> return (s { alex_ust = us }, ())

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = getUserState >>= (setUserState . f)

getCommentDepth :: Alex Int
getCommentDepth = view commentDepth <$> getUserState

setCommentDepth :: Int -> Alex ()
setCommentDepth d = modifyUserState (over commentDepth $ const d)

incrCommentDepth :: Alex ()
incrCommentDepth = getCommentDepth >>= (setCommentDepth . (+) 1)

decrCommentDepth :: Alex ()
decrCommentDepth = getCommentDepth >>= (setCommentDepth . subtract 1)

getStringBuffer :: Alex String
getStringBuffer = view stringBuffer <$> getUserState

setStringBuffer :: String -> Alex ()
setStringBuffer s = modifyUserState (over stringBuffer $ const s)

modifyStringBuffer :: (String -> String) -> Alex ()
modifyStringBuffer f = getStringBuffer >>= (setStringBuffer . f)

getStringStartPosn :: Alex MyPosition
getStringStartPosn = view stringStartPosn <$> getUserState

setStringStartPosn :: MyPosition -> Alex ()
setStringStartPosn p = modifyUserState (over stringStartPosn $ const p)

escapeString :: MyAlexAction
escapeString (_, _, _, (_ : c : _)) _ = do
  let unesc = case c of
                'n' -> '\n'
                't' -> '\t'
                '"' -> '"'
  modifyStringBuffer ((:) unesc)
  return Nothing

adjustLine :: Int -> Int
adjustLine l = l

-- OCaml counts from 0, but Alex counts from 1
adjustCol :: Int -> Int
adjustCol c = c

-- assumes lines and columns have been adjusted!
mkSpan :: MyPosition -> MyPosition -> SrcSpan
mkSpan (sL, sC) (eL, eC) = SrcSpan sL sC eL eC

alexGetPosn :: Alex AlexPosn
alexGetPosn = alex_pos <$> getState

beginString :: MyAlexAction
beginString _ _ = do
  p <- getPos <$> alexGetPosn
  setStringStartPosn p
  alexSetStartCode stringStartCode
  return Nothing

appendString :: MyAlexAction
appendString (_, _, _, (c : _)) _ = do
  modifyStringBuffer ((:) c)
  return Nothing

endString :: MyAlexAction
endString _ _ = do
  str <- getStringBuffer
  startPos <- getStringStartPosn
  endPos <- getPos <$> alexGetPosn
  let span = mkSpan startPos endPos
  setUserState alexInitUserState
  alexSetStartCode codeStartCode
  return $ Just $ Located span (TokString (reverse str, Nothing))

beginComment :: MyAlexAction
beginComment _ _ = do
  incrCommentDepth
  alexSetStartCode commentStartCode
  return Nothing

endComment :: MyAlexAction
endComment _ _ = do
  decrCommentDepth
  cd <- getCommentDepth
  alexSetStartCode $ if cd == 0 then codeStartCode else commentStartCode
  return Nothing

type MyPosition = (Int, Int)

data AlexUserState = AlexUserState
  { _commentDepth    :: !Int
  , _stringBuffer    :: String
  , _stringStartPosn :: !MyPosition
  }
  deriving (Show)

-- Unfortunately, makeLenses breaks alex at the moment, see:
-- https://github.com/simonmar/alex/issues/125
-- makeLenses ''AlexUserState
-- Writing lenses manually instead:
commentDepth :: Lens' AlexUserState Int
commentDepth f s@(AlexUserState { _commentDepth }) =
  (\ d -> s { _commentDepth = d }) <$> f _commentDepth

stringBuffer :: Lens' AlexUserState String
stringBuffer f s@(AlexUserState { _stringBuffer }) =
  (\ b -> s { _stringBuffer = b }) <$> f _stringBuffer

stringStartPosn :: Lens' AlexUserState MyPosition
stringStartPosn f s@(AlexUserState { _stringStartPosn }) =
  (\ p -> s { _stringStartPosn = p }) <$> f _stringStartPosn

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { _commentDepth    = 0
  , _stringBuffer    = ""
  , _stringStartPosn = (0, 0)
  }

}
