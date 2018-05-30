module Language.OCaml.Parser.Tokens
  ( and_T
  , as_T
  , bang_T
  , bar_T
  , colon_T
  , colon_colon_T
  , colon_greater_T
  , comma_T
  , dot_T
  , equal_T
  , false_T
  , function_T
  , in_T
  , int_T
  , l_brace_T
  , l_bracket_T
  , l_bracket_at_at_T
  , l_ident_T
  , l_paren_T
  , let_T
  , minus_T
  , minus_greater_T
  , module_T
  , mutable_T
  , nonrec_T
  , of_T
  , open_T
  , plus_T
  , private_T
  , quote_T
  , r_brace_T
  , r_bracket_T
  , r_paren_T
  , rec_T
  , semi_T
  , semi_semi_T
  , star_T
  , string_T
  , true_T
  , type_T
  , u_ident_T
  , underscore_T
  ) where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.OCaml.Parser.Utils.Utils
import Language.OCaml.Parser.Utils.Types

and_T :: Parser ()
and_T = rword "and"

as_T :: Parser ()
as_T = rword "as"

bang_T :: Parser ()
bang_T = symbol "!"

bar_T :: Parser ()
bar_T = symbol "|"

colon_T :: Parser ()
colon_T = symbol ":"

colon_colon_T :: Parser ()
colon_colon_T = symbol "::"

colon_greater_T :: Parser ()
colon_greater_T = symbol ":>"

comma_T :: Parser ()
comma_T = symbol ","

dot_T :: Parser ()
dot_T = symbol "."

equal_T :: Parser ()
equal_T = symbol "="

false_T :: Parser ()
false_T = rword "false"

function_T :: Parser ()
function_T = rword "function"

binDigitChar :: Parser Char
binDigitChar = char '0' <|> char '1'

bin_literal_P :: Parser String
bin_literal_P = do
  _ <- char '0'
  _ <- char 'b' <|> char 'B'
  h <- binDigitChar
  t <- many (binDigitChar <|> char '_')
  return $ h : t

decimal_literal_P :: Parser String
decimal_literal_P = do
  h <- digitChar
  t <- many (digitChar <|> char '_')
  return $ h : t

hex_literal_P :: Parser String
hex_literal_P = do
  _ <- char '0'
  _ <- char 'x' <|> char 'X'
  h <- hexDigitChar
  t <- many (hexDigitChar <|> char '_')
  return $ h : t

oct_literal_P :: Parser String
oct_literal_P = do
  _ <- char '0'
  _ <- char 'o' <|> char 'O'
  h <- octDigitChar
  t <- many (octDigitChar <|> char '_')
  return $ h : t

int_literal_P :: Parser String
int_literal_P = choice
  [ decimal_literal_P
  , hex_literal_P
  , oct_literal_P
  , bin_literal_P
  ]

in_T :: Parser ()
in_T = rword "in"

int_T :: Parser (String, Maybe Char)
int_T = do
  l <- int_literal_P
  m <- literal_modifier_P
  return $ (l, Just m)

isBetween :: Char -> Char -> Char -> Bool
isBetween cmin c cmax = (vmin <= v && v <= vmax)
  where
    vmin = conv cmin
    v    = conv c
    vmax = conv cmax
    conv = ord

isModifier :: Char -> Bool
isModifier c = isBetween 'G' c 'Z' || isBetween 'g' c 'z'

literal_modifier_P :: Parser Char
literal_modifier_P = satisfy isModifier

ident_char_T :: Parser Char
ident_char_T = choice
  [ upperChar
  , lowerChar
  , char '_'
  , char '\''
  , digitChar
  ]

l_brace_T :: Parser ()
l_brace_T = symbol "{"

l_bracket_T :: Parser ()
l_bracket_T = symbol "["

l_bracket_at_at_T :: Parser ()
l_bracket_at_at_T = symbol "[@@"

l_ident_T :: Parser String
l_ident_T = identifier $ do
  c <- lowerChar
  cs <- many ident_char_T
  return $ c : cs

l_paren_T :: Parser ()
l_paren_T = symbol "("

let_T :: Parser ()
let_T = rword "let"

nonrec_T :: Parser ()
nonrec_T = rword "nonrec"

minus_T :: Parser ()
minus_T = symbol "-"

minus_greater_T :: Parser ()
minus_greater_T = symbol "->"

module_T :: Parser ()
module_T = rword "module"

mutable_T :: Parser ()
mutable_T = rword "mutable"

of_T :: Parser ()
of_T = rword "of"

open_T :: Parser ()
open_T = rword "open"

plus_T :: Parser ()
plus_T = symbol "+"

private_T :: Parser ()
private_T = rword "private"

quote_T :: Parser ()
quote_T = symbol "'"

r_brace_T :: Parser ()
r_brace_T = symbol "}"

r_bracket_T :: Parser ()
r_bracket_T = symbol "]"

r_paren_T :: Parser ()
r_paren_T = symbol ")"

rec_T :: Parser ()
rec_T = rword "rec"

semi_T :: Parser ()
semi_T = symbol ";"

semi_semi_T :: Parser ()
semi_semi_T = symbol ";;"

star_T :: Parser ()
star_T = symbol "*"

-- FIXME: real strings have nesting comments and escape characters
string_T :: Parser (String, Maybe String)
string_T = lexeme $ do
  _ <- char '"'
  s <- many $ satisfy ((/=) '"')
  _ <- char '"'
  return (s, Nothing)

true_T :: Parser ()
true_T = rword "true"

type_T :: Parser ()
type_T = rword "type"

u_ident_T :: Parser String
u_ident_T = identifier $ do
  c <- upperChar
  cs <- many ident_char_T
  return $ c : cs

underscore_T :: Parser ()
underscore_T = symbol "_"
