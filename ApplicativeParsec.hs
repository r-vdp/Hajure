
module ApplicativeParsec
  ( module Control.Applicative
  , module Text.Parsec.Prim
  , module Text.ParserCombinators.Parsec
  ) where

import Control.Applicative

import Text.Parsec.Prim hiding (many, try, runParser, (<|>))
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

