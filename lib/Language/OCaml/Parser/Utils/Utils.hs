module Language.OCaml.Parser.Utils.Utils
  -- , checkIdentifier
  ( identifier
  , lexeme
  , ocamlSpace
  , parens
  , rword
  , symbol
  ) where

import           Data.Functor
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import Language.OCaml.Parser.Utils.Types

checkIdentifier :: String -> Parser String
checkIdentifier c =
      if c `elem` reservedWords
      then fail $ printf "keyword %s cannot be an identifier" (show c)
      else return c

identifier :: Parser String -> Parser String
identifier p = (lexeme . try) (p >>= checkIdentifier)

-- identifier :: Parser String
-- identifier = (lexeme . try) (p >>= checkIdentifier)
--   where
--     p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

ocamlSpace :: Parser ()
ocamlSpace = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ocamlSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w =
  if w `elem` reservedWords
  then string w *> notFollowedBy alphaNumChar *> ocamlSpace
  else error
       $ "The following word must be added to reservedWords in Language.OCaml.Parser.Utils.Utils: "
       ++ w

symbol :: String -> Parser ()
symbol = void . L.symbol ocamlSpace

reservedWords :: [String] -- list of reserved words
reservedWords =
  [ "and"
  , "as"
  , "assert"
  , "begin"
  , "class"
  , "constraint"
  , "do"
  , "done"
  , "downto"
  , "else"
  , "end"
  , "exception"
  , "external"
  , "false"
  , "for"
  , "fun"
  , "function"
  , "functor"
  , "if"
  , "in"
  , "include"
  , "inherit"
  , "initializer"
  , "lazy"
  , "let"
  , "match"
  , "method"
  , "module"
  , "mutable"
  , "new"
  , "nonrec"
  , "object"
  , "of"
  , "open"
  , "or"
  -- , "parser"
  , "private"
  , "rec"
  , "sig"
  , "struct"
  , "then"
  , "to"
  , "true"
  , "try"
  , "type"
  , "val"
  , "virtual"
  , "when"
  , "while"
  , "with"
  , "lor"
  , "lxor"
  , "mod"
  , "land"
  , "lsl"
  , "lsr"
  , "asr"
  ]
