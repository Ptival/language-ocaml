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

import           Control.Lens hiding (none)
import           Control.Lens.TH
import           Control.Monad.State
import qualified Data.Map.Strict as M
import           GHC.Generics
import           Text.Printf

import           Language.OCaml.Definitions.Parsing.Location
import           Language.OCaml.Utils

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
  <0> "~" lowerCase identChar* ":"                        { tokS $ TokLabel . getLabelName }
  <0> "?"                                                 { tok  TokQuestion }
  <0> "?" lowerCase identChar* ":"                        { tokS $ TokOptLabel . getLabelName }
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
  <0> \"                                                  { beginString }
  <0> "(*"                                                { beginCommentBuffer False "" }
  <0> "(**"                                               { beginCommentBuffer True "*"}
  <0> "(**" "*"+                                          { \ alexInput lexemeLength ->
                                                            let nbDropped = length "(**" in
                                                            beginCommentBuffer False (drop nbDropped (getLexeme alexInput lexemeLength)) alexInput lexemeLength
                                                          }
  <0> "(*" "*"* "*)"                                      { \ alexInput lexemeLength -> do
                                                            ss <- getLexemeSpan alexInput lexemeLength
                                                            case getLexeme alexInput lexemeLength of
                                                              "(**)" -> return $ Just $ Located ss $ TokDocstring "(**)"
                                                              lexeme -> return $ Just $ Located ss $ TokComment lexeme
                                                          }
  <0> "*)"                                                { tokStarHack }

  <0> "#"                                                 { tok TokHash {- TODO: directives -} }
  <0> "&"                                                 { tok TokAmpersand }
  <0> "&&"                                                { tok TokAmperAmper }
  <0> "`"                                                 { tok TokBackQuote }
  <0> "'"                                                 { tok TokQuote }
  <0> "("                                                 { tok TokLParen }
  <0> ")"                                                 { tok TokRParen }
  <0> "*"                                                 { tok TokStar }
  <0> ","                                                 { tok TokComma }
  <0> "->"                                                { tok TokMinusGreater }
  <0> "."                                                 { tok TokDot }
  <0> ".."                                                { tok TokDotDot }
  -- TODO
  <0> ":"                                                 { tok TokColon }
  <0> "::"                                                { tok TokColonColon }
  <0> ":="                                                { tok TokColonEqual }
  <0> ":>"                                                { tok TokColonGreater }
  <0> ";"                                                 { tok TokSemi }
  <0> ";;"                                                { tok TokSemiSemi }
  <0> "<"                                                 { tok TokLess }
  <0> "<-"                                                { tok TokLessMinus }
  <0> "="                                                 { tok TokEqual }
  <0> "["                                                 { tok TokLBracket }
  <0> "[|"                                                { tok TokLBracketBar }
  <0> "[<"                                                { tok TokLBracketLess }
  <0> "[>"                                                { tok TokLBracketGreater }
  <0> "]"                                                 { tok TokRBracket }
  <0> "{"                                                 { tok TokLBrace }
  <0> "{<"                                                { tok TokLBraceLess }
  <0> "|"                                                 { tok TokBar }
  <0> "||"                                                { tok TokBarBar }
  <0> "|]"                                                { tok TokBarRBracket }
  <0> ">"                                                 { tok TokGreater }
  <0> ">]"                                                { tok TokGreaterRBracket }
  <0> "}"                                                 { tok TokRBrace }
  <0> ">}"                                                { tok TokGreaterRBrace }
  <0> "[@"                                                { tok TokLBracketAt }
  <0> "[@@"                                               { tok TokLBracketAtAt }
  <0> "[@@@"                                              { tok TokLBracketAtAtAt }
  <0> "[%"                                                { tok TokLBracketPercent }
  <0> "[%%"                                               { tok TokLBracketPercentPercent }
  <0> "!"                                                 { tok TokBang }
  <0> "!="                                                { tok (TokInfixOp0 "!=") }
  <0> "+"                                                 { tok TokPlus }
  <0> "+."                                                { tok TokPlusDot }
  <0> "+="                                                { tok TokPlusEq }
  <0> "-"                                                 { tok TokMinus }
  <0> "-."                                                { tok TokMinusDot }
  <0> "!" $symbolChar+                                    { tokS TokPrefixOp }
  -- TODO
  <0> [= \< > \| & \$] $symbolChar*                       { tokS TokInfixOp0 }

  -- while inside a string
  <s> \"                                                  { endString }
  <s> .                                                   { appendString }
  <s> \\[nt\"]                                            { escapeString }

  -- while inside a comment
  <c> "(*"                                                { beginInnerComment False }
  <c> "*)"                                                { endComment }
  <c> [.\n]                                               { storeLexeme }

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
  | TokComment String
  | TokConstraint
  | TokDo
  | TokDocstring String
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
  | TokHash
  | TokHashOp String
  | TokIf
  | TokIn
  | TokInclude
  | TokInfixOp0 String
  | TokInfixOp1 String
  | TokInfixOp2 String
  | TokInfixOp3 String
  | TokInfixOp4 String
  | TokInherit
  | TokInitializer
  | TokInt (String, (Maybe Char))
  | TokLabel String
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
  | TokOptLabel String
  | TokOr
  | TokPercent
  | TokPlus
  | TokPlusDot
  | TokPlusEq
  | TokPrefixOp String
  | TokPrivate
  | TokQuestion
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
  (l, c) <- getPositionAfterLexeme
  let ss = SrcSpan l c l c
  return $ Just $ Located ss TokEOF

myAlexMonadScan :: Alex ResultToken
myAlexMonadScan = alexMonadScan >>= \case
  Nothing -> myAlexMonadScan
  Just t -> case t of
    -- TODO: instead of dropping comments, integrate them
    Located _ (TokComment _)   -> myAlexMonadScan
    Located _ (TokDocstring _) -> myAlexMonadScan
    _                          -> return t

lexWrap :: (ResultToken -> Alex a) -> Alex a
lexWrap cont = myAlexMonadScan >>= cont

alexScanTokens :: String -> Either String [ResultToken]
alexScanTokens input = runAlex input gather
  where
    gather :: Alex [ResultToken]
    gather = do
      myAlexMonadScan >>= \case
        t@(Located _ TokEOF)           -> return [t]
        t                              -> return . (t :) =<< gather

alexPosnToMyPosition :: AlexPosn -> MyPosition
-- NOTE: OCaml starts at column 0, but Alex starts at column 1
alexPosnToMyPosition (AlexPn _ line col) = (adjustLine line, adjustCol col)

getPositionAfterLexeme :: Alex MyPosition
getPositionAfterLexeme = Alex $ \ s -> Right (s, alexPosnToMyPosition . alex_pos $ s)

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

getLexemeSpan :: AlexInput -> Int -> Alex SrcSpan
getLexemeSpan alexInput lexemeLength = do
  let (l, c) = getMyPosition alexInput
  return $ SrcSpan l c l (c + lexemeLength)

tok :: Token -> MyAlexAction
tok t alexInput lexemeLength = do
  ss <- getLexemeSpan alexInput lexemeLength
  return $ Just $ Located ss t

tokS :: (String -> Token) -> MyAlexAction
tokS t input@(_, _, _, str) len = tok (t $ take len str) input len

extractModifier :: String -> (String, Char)
extractModifier s = case reverse s of
  mod : lit -> (lit, mod)
  _         -> error "This should not happen"

getAlexState :: Alex AlexState
getAlexState = Alex $ \ s -> return (s, s)

getAlexUserState :: Alex AlexUserState
getAlexUserState = Alex $ \ s -> return (s, alex_ust s)

setAlexState :: AlexState -> Alex ()
setAlexState s = Alex $ const $ Right (s, ())

setAlexUserState :: AlexUserState -> Alex ()
setAlexUserState us = Alex $ \ s -> return (s { alex_ust = us }, ())

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = getAlexUserState >>= (setAlexUserState . f)

getCommentStartLoc :: Alex [MyPosition]
getCommentStartLoc = view commentStartLoc <$> getAlexUserState

setCommentStartLoc :: [MyPosition] -> Alex ()
setCommentStartLoc l = modifyUserState (over commentStartLoc $ const l)

modifyCommentStartLoc :: ([MyPosition] -> [MyPosition]) -> Alex ()
modifyCommentStartLoc f = getCommentStartLoc >>= (setCommentStartLoc . f)

pushCommentStartLoc :: MyPosition -> Alex ()
pushCommentStartLoc l = modifyCommentStartLoc ((:) l)

popCommentStartLoc :: Alex ()
popCommentStartLoc = modifyCommentStartLoc tail

getStringBuffer :: Alex String
getStringBuffer = view stringBuffer <$> getAlexUserState

setStringBuffer :: String -> Alex ()
setStringBuffer s = modifyUserState (over stringBuffer $ const s)

resetStringBuffer :: Alex ()
resetStringBuffer = setStringBuffer ""

modifyStringBuffer :: (String -> String) -> Alex ()
modifyStringBuffer f = getStringBuffer >>= (setStringBuffer . f)

appendToStringBuffer :: String -> Alex ()
appendToStringBuffer s = modifyStringBuffer (++ s)

getStringStartPosn :: Alex MyPosition
getStringStartPosn = view stringStartPosn <$> getAlexUserState

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
alexGetPosn = alex_pos <$> getAlexState

beginString :: MyAlexAction
beginString alexInput _ = do
  setStringStartPosn $ getMyPosition alexInput
  alexSetStartCode stringStartCode
  return Nothing

appendString :: MyAlexAction
appendString (_, _, _, (c : _)) _ = do
  modifyStringBuffer ((:) c)
  return Nothing

endString :: MyAlexAction
endString alexInput _ = do
  str <- getStringBuffer
  startPos <- getStringStartPosn
  let endPos = getMyPosition alexInput
  let span = mkSpan startPos endPos
  setAlexUserState alexInitUserState
  alexSetStartCode codeStartCode
  return $ Just $ Located span (TokString (reverse str, Nothing))

handleDocstrings :: Bool
handleDocstrings = True

beginCommentBuffer :: Bool -> String -> MyAlexAction
beginCommentBuffer isDocstring stringBuffer alexInput lexemeLength = do
  let lexeme = getLexeme alexInput lexemeLength
  setIsDocstringTo isDocstring
  setCommentStartLoc [getMyPosition alexInput]
  setStringBuffer stringBuffer
  alexSetStartCode commentStartCode
  return Nothing

beginInnerComment :: Bool -> MyAlexAction
beginInnerComment isDocstring alexInput _ = do
  pushCommentStartLoc $ getMyPosition alexInput
  return Nothing

endComment :: MyAlexAction
endComment alexInput lexemeLength = do
  l <- getCommentStartLoc
  case l of
    []  -> error "This should not happen"
    [(startLine, startCol)] -> do
      popCommentStartLoc
      commentString <- getStringBuffer
      let (endLine, endCol) = getMyPosition alexInput
      let ss = SrcSpan startLine startCol endLine endCol
      return $ Just $ Located ss $ TokComment commentString
    _ -> do
      popCommentStartLoc
      appendToStringBuffer $ getLexeme alexInput lexemeLength
      return Nothing

-- getInputAfterLexeme :: Alex String
-- getInputAfterLexeme = alex_inp <$> getAlexState

getPosition :: AlexInput -> AlexPosn
getPosition (pos, _, _, _) = pos

getMyPosition :: AlexInput -> MyPosition
getMyPosition = alexPosnToMyPosition . getPosition

getLexeme :: AlexInput -> Int -> String
getLexeme (_, _, _, str) len = take len str

storeLexeme :: MyAlexAction
storeLexeme inp len = do
  appendToStringBuffer $ getLexeme inp len
  return Nothing

type MyPosition = (Int, Int)

data AlexUserState = AlexUserState
  { _commentStartLoc :: ![MyPosition]
  , _isDocstring     :: Bool
  , _stringBuffer    :: String
  , _stringStartPosn :: !MyPosition
  }
  deriving (Show)

-- Unfortunately, makeLenses breaks alex at the moment, see:
-- https://github.com/simonmar/alex/issues/125
-- makeLenses ''AlexUserState
-- Writing lenses manually instead:
commentStartLoc :: Lens' AlexUserState [MyPosition]
commentStartLoc f s@(AlexUserState { _commentStartLoc }) =
  (\ l -> s { _commentStartLoc = l }) <$> f _commentStartLoc

isDocstring :: Lens' AlexUserState Bool
isDocstring f s@(AlexUserState { _isDocstring }) =
  (\ b -> s { _isDocstring = b }) <$> f _isDocstring

setIsDocstringTo :: Bool -> Alex ()
setIsDocstringTo b = modifyUserState $ over isDocstring $ const b

setDocstring :: Alex ()
setDocstring = setIsDocstringTo True

unsetDocstring :: Alex ()
unsetDocstring = setIsDocstringTo False

stringBuffer :: Lens' AlexUserState String
stringBuffer f s@(AlexUserState { _stringBuffer }) =
  (\ b -> s { _stringBuffer = b }) <$> f _stringBuffer

stringStartPosn :: Lens' AlexUserState MyPosition
stringStartPosn f s@(AlexUserState { _stringStartPosn }) =
  (\ p -> s { _stringStartPosn = p }) <$> f _stringStartPosn

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { _commentStartLoc = []
  , _stringBuffer    = ""
  , _stringStartPosn = (0, 0)
  }

getLabelName :: String -> String
getLabelName s =
  let name = slice 1 (- 2) s in
  if M.member name keywordTable
  then error $ "You cannot use this keyword as a label: " ++ name
  else name

-- inclusive on both ends
slice :: Int -> Int -> [a] -> [a]
slice from to l | from > 0 && to > 0 = slicePositive from to l
                | from > 0 && to < 0 = slicePositive from (length l - 1 - to) l
                | from < 0 && to > 0 = error "TODO"
                | otherwise          = error "TODO"

slicePositive :: Int -> Int -> [a] -> [a]
slicePositive from to | from >= 0 && to >= 0 = take (to - from) . drop from
                      | otherwise            = error "slicePositive: negative indices"

{- OCaml parses input like "( *)" as a multiplication. The way it does it, is
that it treats the open parenthesis as LPAREN, then upon seeing "*)" in a
non-comment context, it outputs the token STAR after having rewound the lexer
state to before the closing parenthesis, so that the lexer then outputs a RPAREN
token too.  Emulating this hack...
-}
tokStarHack :: MyAlexAction
tokStarHack alexInput _ = do
  s <- getAlexState
  setAlexState $ AlexState
    { alex_pos   = rewind $ alex_pos s
    , alex_inp   = ')' : alex_inp s
    , alex_chr   = '*'
    , alex_bytes = []
    , alex_scd   = alex_scd s
    }
  -- the lexeme length should be 2, but we just want the '*'
  ss <- getLexemeSpan alexInput 1
  return $ Just $ Located ss TokStar
  where
    -- rewinds a normal character (not \t or \n)
    rewind (AlexPn a l c) = AlexPn (a - 1) l (c - 1)

}
