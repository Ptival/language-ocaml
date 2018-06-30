module Language.OCaml.Parser.Tokens
  ( andT
  , asT
  , atT
  , bangT
  , barT
  , barRBracketT
  , caretT
  , charT
  , colonT
  , colonColonT
  , colonGreaterT
  , commaT
  , dotT
  , elseT
  , equalT
  , exceptionT
  , falseT
  , funT
  , functionT
  , greaterT
  , ifT
  , inT
  , intT
  , lBraceT
  , lBracketT
  , lBracketAtT
  , lBracketAtAtT
  , lBracketBarT
  , lIdentT
  , lParenT
  , letT
  , matchT
  , minusT
  , minusGreaterT
  , moduleT
  , mutableT
  , nonrecT
  , ofT
  , openT
  , plusT
  , privateT
  , quoteT
  , rBraceT
  , rBracketT
  , rParenT
  , recT
  , semiT
  , semiSemiT
  , starT
  , stringT
  , thenT
  , trueT
  , typeT
  , uIdentT
  , underscoreT
  , whenT
  , withT
  ) where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.OCaml.Parser.Utils.Utils
import Language.OCaml.Parser.Utils.Types

andT :: Parser ()
andT = rword "and"

asT :: Parser ()
asT = rword "as"

atT :: Parser ()
atT = symbol "@"

bangT :: Parser ()
bangT = symbol "!"

barT :: Parser ()
barT = symbol "|"

barRBracketT :: Parser ()
barRBracketT = symbol "|]"

binDigitChar :: Parser Char
binDigitChar = char '0' <|> char '1'

binLiteralP :: Parser String
binLiteralP = do -- WARNING: do not add `lexeme` here
  _ <- char '0'
  _ <- char 'b' <|> char 'B'
  h <- binDigitChar
  t <- many (binDigitChar <|> char '_')
  return $ h : t

caretT :: Parser ()
caretT = symbol "^"

charT :: Parser Char
charT = do
  symbol "'"
  c <- satisfy (not . (`elem` ['\\', '\'', '\010', '\013']))
  symbol "'"
  return c

colonT :: Parser ()
colonT = symbol ":"

colonColonT :: Parser ()
colonColonT = symbol "::"

colonGreaterT :: Parser ()
colonGreaterT = symbol ":>"

commaT :: Parser ()
commaT = symbol ","

decimalLiteralP :: Parser String
decimalLiteralP = do -- WARNING: do not add `lexeme` here
  h <- digitChar
  t <- many (digitChar <|> char '_')
  return $ h : t

dotT :: Parser ()
dotT = symbol "."

elseT :: Parser ()
elseT = rword "else"

equalT :: Parser ()
equalT = symbol "="

falseT :: Parser ()
falseT = rword "false"

funT :: Parser ()
funT = rword "fun"

functionT :: Parser ()
functionT = rword "function"

ifT :: Parser ()
ifT = rword "if"

exceptionT :: Parser ()
exceptionT = rword "exception"

greaterT :: Parser ()
greaterT = symbol ">"

hexLiteralP :: Parser String
hexLiteralP = do -- WARNING: do not add `lexeme` here
  _ <- char '0'
  _ <- char 'x' <|> char 'X'
  h <- hexDigitChar
  t <- many (hexDigitChar <|> char '_')
  return $ h : t

intLiteralP :: Parser String
intLiteralP = choice
  [ decimalLiteralP
  , hexLiteralP
  , octLiteralP
  , binLiteralP
  ]

inT :: Parser ()
inT = rword "in"

intT :: Parser (String, Maybe Char)
intT = lexeme $ do
  l <- intLiteralP
  m <- choice
    [ Just <$> literalModifierP
    , return Nothing
    ]
  return (l, m)

isBetween :: Char -> Char -> Char -> Bool
isBetween cmin c cmax = (vmin <= v && v <= vmax)
  where
    vmin = conv cmin
    v    = conv c
    vmax = conv cmax
    conv = ord

isModifier :: Char -> Bool
isModifier c = isBetween 'G' c 'Z' || isBetween 'g' c 'z'

literalModifierP :: Parser Char
literalModifierP = satisfy isModifier

identCharT :: Parser Char
identCharT = choice
  [ upperChar
  , lowerChar
  , char '_'
  , char '\''
  , digitChar
  ]

lBraceT :: Parser ()
lBraceT = symbol "{"

lBracketT :: Parser ()
lBracketT = symbol "["

lBracketAtT :: Parser ()
lBracketAtT = symbol "[@"

lBracketAtAtT :: Parser ()
lBracketAtAtT = symbol "[@@"

lBracketBarT :: Parser ()
lBracketBarT = symbol "[|"

lIdentT :: Parser String
lIdentT = identifier $ do
  c <- lowerChar
  cs <- many identCharT
  return $ c : cs

lParenT :: Parser ()
lParenT = symbol "("

letT :: Parser ()
letT = rword "let"

matchT :: Parser ()
matchT = rword "match"

minusT :: Parser ()
minusT = symbol "-"

minusGreaterT :: Parser ()
minusGreaterT = symbol "->"

moduleT :: Parser ()
moduleT = rword "module"

mutableT :: Parser ()
mutableT = rword "mutable"

nonrecT :: Parser ()
nonrecT = rword "nonrec"

octLiteralP :: Parser String
octLiteralP = do -- WARNING: do not add `lexeme` here
  _ <- char '0'
  _ <- char 'o' <|> char 'O'
  h <- octDigitChar
  t <- many (octDigitChar <|> char '_')
  return $ h : t

ofT :: Parser ()
ofT = rword "of"

openT :: Parser ()
openT = rword "open"

plusT :: Parser ()
plusT = symbol "+"

privateT :: Parser ()
privateT = rword "private"

quoteT :: Parser ()
quoteT = symbol "'"

rBraceT :: Parser ()
rBraceT = symbol "}"

rBracketT :: Parser ()
rBracketT = symbol "]"

rParenT :: Parser ()
rParenT = symbol ")"

recT :: Parser ()
recT = rword "rec"

semiT :: Parser ()
semiT = symbol ";"

semiSemiT :: Parser ()
semiSemiT = symbol ";;"

starT :: Parser ()
starT = symbol "*"

-- FIXME: real strings have nesting comments and escape characters
stringT :: Parser (String, Maybe String)
stringT = lexeme $ do
  _ <- char '"'
  s <- many $ satisfy ((/=) '"')
  _ <- char '"'
  return (s, Nothing)

thenT :: Parser ()
thenT = rword "then"

trueT :: Parser ()
trueT = rword "true"

typeT :: Parser ()
typeT = rword "type"

uIdentT :: Parser String
uIdentT = identifier $ do
  c <- upperChar
  cs <- many identCharT
  return $ c : cs

underscoreT :: Parser ()
underscoreT = symbol "_"

whenT :: Parser ()
whenT = rword "when"

withT :: Parser ()
withT = rword "with"
