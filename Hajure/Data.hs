
module Hajure.Data where

import Control.Monad
import Data.List

data Element a = Nested (SExpr a)
               | Ident  a
               | Num    a
               | Op     a
               | List   [Element a]
  deriving Show

newtype SExpr a = SExpr { unwrap :: [Element a] }

instance Show a => Show (SExpr a) where
  show (SExpr xs) = "S( " ++ showElements xs ++ " )"
    where showElements = join . intersperse " " . embrace
          embrace      = map (\x -> "{" ++ show x ++ "}")

