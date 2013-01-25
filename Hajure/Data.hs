
module Hajure.Data (Element(..), SExpr(..), TextElem) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.List (intercalate)
import Data.Text.Lazy (Text)

data Element a = Nested (SExpr a)
               | Ident  a
               | Num    a
               | Op     a
               | List   [Element a]
  deriving Show

newtype SExpr a = SExpr { unwrap :: [Element a] }

instance Show a => Show (SExpr a) where
  show = unlines . join showExpr ""

type TextElem = Element Text


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

showExpr' :: Show a => String -> SExpr a -> [String]
showExpr' = liftA2 showExpr withChld withNxt

showElem :: Show a => String -> Element a -> String
showElem acc (List as)  = showAsChild acc as
showElem acc (Nested e) = unlines' . showExpr' acc $ e
showElem acc e          = showAsChild acc e

unlines' :: [String] -> String
unlines' = intercalate "\n"

showAsChild :: Show a => String -> a -> String
showAsChild acc a = withChld acc ++ show a

