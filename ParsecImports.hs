
module ParsecImports (module X) where

import Control.Applicative as X

import Text.Parsec.Prim as X hiding (many, try, (<|>))
import Text.ParserCombinators.Parsec as X hiding (many, optional, (<|>), Parser, GenParser, runParser)
import Text.Parsec.Text as X

