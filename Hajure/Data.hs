
module Hajure.Data (Element(..), SExpr(..)) where

import Control.Applicative (liftA2)
import Data.List (intercalate)

data Element a = Nested (SExpr a)
               | Ident  a
               | Num    a
               | Op     a
               | List   [Element a]
  deriving Show

newtype SExpr a = SExpr { unwrap :: [Element a] }

instance Show a => Show (SExpr a) where
  show = unlines . showExpr "" ""

chld :: String
chld = "|-- "

nxt :: String
nxt  = "|   "

withChld :: String -> String
withChld = (++ chld)

withNxt :: String -> String
withNxt  = (++ nxt)

showExpr :: Show a => String -> String -> SExpr a -> [String]
showExpr acc1 acc2 e = start : body e ++ end
  where start = acc1 ++ "S("
        body  = map (showElem acc2) . unwrap
        end   = [acc2 ++ ")"]

showElem :: Show a => String -> Element a -> String
showElem acc (Ident a)  = showAsChild acc a
showElem acc (Num a)    = showAsChild acc a
showElem acc (Op a)     = showAsChild acc a
showElem acc (List as)  = showAsChild acc as
showElem acc (Nested e) = unlines' . show' acc $ e
  where show'    = liftA2 showExpr withChld withNxt
        unlines' = intercalate "\n"

showAsChild :: Show a => String -> a -> String
showAsChild acc a = withChld acc ++ show a

