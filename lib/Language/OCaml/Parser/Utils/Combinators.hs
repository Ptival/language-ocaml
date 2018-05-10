module Language.OCaml.Parser.Utils.Combinators
  ( chainl1
  , chainl1'
  , chainl1try
  , chainl1try'
  , leftRecursive
  ) where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

{-

This combinator is necessary because we want to parse succesfully even when
the `op` is not part of the chain.

For instance, in OCaml, a mod_longident is a sequence of `Foo.Bar.Baz` and must
be parsed as a chain.  However, when presented with `Foo.Bar.baz`, we'd like the
chain to capture `Foo.Bar` and ignore the `.baz`.

-}
chainl1try' :: Parser a -> Parser (b -> a -> b) -> (a -> b) -> Parser b
chainl1try' p op bc = p >>= rest . bc
  where
    rest x = tryOp x <|> return x
    tryOp x = try $ do
      f <- op
      y <- p
      rest (f x y)

chainl1try :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl1try p op = chainl1try' p op id

chainl1' :: (Alternative m, Monad m) => m a -> m (b -> a -> b) -> (a -> b) -> m b
chainl1' p op bc = p >>= rest . bc
  where
    rest x = do { f <- op ; y <- p ; rest (f x y) } <|> return x

chainl1 :: (Alternative m, Monad m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = chainl1' p op id

{-

This applies the procedure of turning a left-recursive grammar into a
non-left-recursive one.  Essentially, when wanting to write the grammar:

A ::=
| A B { foo A B }
| C   { bar C }

One instead writes the following:

A' ::=
| C R       { R (bar C) }

R ::=
| B R       { \ A -> R (foo A B) }
| {-empty-} { id }

The two arguments to `leftRecursive` are the grammars for A' and R (except the
last line of R is not needed, but added by the combinator).

-}
leftRecursive :: (Monad m, Alternative m) => [m a] -> [m (a -> a)] -> m a
leftRecursive prefixes suffixes = choice $ map patchPrefix prefixes
  where
    patchPrefix prefixP = do
      p <- prefixP
      r <- rest
      return $ r p
    patchSuffix suffixP = do
      s <- suffixP
      r <- rest
      return $ r . s
    rest = choice $ map patchSuffix suffixes ++ [ return id ]
