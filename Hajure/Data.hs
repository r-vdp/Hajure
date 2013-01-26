
module Hajure.Data (Element(..), SExpr(..)) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.List (intercalate)
import Data.Text (Text)

data Element = Nested SExpr
             | Ident  Text
             | Num    Double
             | Op     Text
             | List   [Element]
  deriving (Show, Eq)

newtype SExpr = SExpr { unwrap :: [Element] }
  deriving Eq

instance Show SExpr where
  show = unlines . join showExpr ""


chld, nxt :: String
chld = "|-- "
nxt  = "|   "

withChld :: String -> String
withChld = (++ chld)

withNxt :: String -> String
withNxt  = (++ nxt)

showExpr :: String -> String -> SExpr -> [String]
showExpr acc1 acc2 e = start : body e ++ end
  where start = acc1 ++ "S("
        body  = map (showElem acc2) . unwrap
        end   = [acc2 ++ ")"]

showExpr' :: String -> SExpr -> [String]
showExpr' = liftA2 showExpr withChld withNxt

showElem :: String -> Element -> String
showElem acc (List es)  = showAsChild acc es
showElem acc (Nested e) = unlines' . showExpr' acc $ e
showElem acc e          = showAsChild acc e

unlines' :: [String] -> String
unlines' = intercalate "\n"

showAsChild :: Show a => String -> a -> String
showAsChild acc a = withChld acc ++ show a

