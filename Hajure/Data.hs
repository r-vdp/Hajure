
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

showElem :: String -> Element -> String
showElem acc (List es)  = showList    acc es
showElem acc (Nested e) = showExpr'   acc e
showElem acc e          = showAsChild acc e

showAsChild :: Show a => String -> a -> String
showAsChild acc a = withChld acc ++ show a

showWith :: (String -> String -> a -> [String]) -> String -> a -> String
showWith f acc   = unlines' . lifted acc
  where lifted   = liftA2 f withChld withNxt
        unlines' = intercalate "\n"

showExpr' :: String -> SExpr -> String
showExpr' = showWith showExpr

showList :: String -> [Element] -> String
showList = showWith showListElems

showListElems :: String -> String -> [Element] -> [String]
showListElems acc1 acc2 es = start : showElems es ++ end
  where showElems = map (showElem acc2)
        start     = acc1 ++ "["
        end       = [acc2 ++ "]"]

