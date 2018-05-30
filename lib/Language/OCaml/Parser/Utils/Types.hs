module Language.OCaml.Parser.Utils.Types
  ( Parser
  ) where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String
