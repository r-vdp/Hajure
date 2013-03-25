
module ParsecImports (module X) where

import Control.Applicative as X

import Text.Parsec      as X hiding (many, optional, (<|>))
import Text.Parsec.Prim as X hiding (many, try, (<|>))
import Text.Parsec.Text as X

