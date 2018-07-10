module Language.OCaml.Utils
  ( (<*^>)
  ) where

infixl 4 <*^>

-- like <*>, but lifts the incoming argument
(<*^>) :: (Applicative f) => f (a -> b) -> a -> f b
(<*^>) f a = f <*> pure a
