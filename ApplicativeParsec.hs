
module ApplicativeParsec (module X) where

import Control.Applicative as X

import Text.Parsec.Prim as X hiding (many, try, runParser, (<|>))
import Text.ParserCombinators.Parsec as X hiding (many, optional, (<|>), Parser, GenParser)
import Text.Parsec.Text as X

