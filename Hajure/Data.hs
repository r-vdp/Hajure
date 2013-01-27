
module Hajure.Data
  ( Element(..)
  , SExpr(..)
  ) where

import Prelude hiding (showList)

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

showExpr' :: String -> SExpr -> String
showExpr' acc  = unlines' . lifted acc
  where lifted = liftA2 showExpr withChld withNxt

showElem :: String -> Element -> String
showElem acc (List es)  = showList    acc es
showElem acc (Nested e) = showExpr'   acc e
showElem acc e          = showAsChild acc e

unlines' :: [String] -> String
unlines' = intercalate "\n"

showAsChild :: Show a => String -> a -> String
showAsChild acc a = withChld acc ++ show a

showList :: String -> [Element] -> String
showList acc = unlines' . showListElems (acc ++ chld) (acc ++ nxt)

showListElems :: String -> String -> [Element] -> [String]
showListElems acc1 acc2 es = start : showElems es ++ end
  where showElems = map (showElem acc2)
        start     = acc1 ++ "["
        end       = [acc2 ++ "]"]


