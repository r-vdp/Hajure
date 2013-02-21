
module Hajure.Data
  ( HParser
  , Element(..)
  , SExpr(..)
  , PrettyShow
  , pshow
  ) where

import Prelude hiding (showList)

import Control.Monad (join)
import Data.List (intercalate)
import Data.Text (Text, unpack)

import ParsecImports

type HParser = Parsec Text ()


data Element = Nested SExpr
             | Ident  Text
             | Num    Double
             | Op     Text
             | List   [Element]
  deriving (Eq, Show)

newtype SExpr = SExpr { unwrap :: [Element] }
  deriving (Eq, Show)


class PrettyShow a where
  pshow :: a -> String

instance PrettyShow Element where
  pshow (Nested s) = pshow s
  pshow (List   l) = emptyAcc showListElems l
  pshow (Ident  i) = "Ident " ++ unpack i
  pshow (Num    n) = "Num "   ++ show   n
  pshow (Op     o) = "Op "    ++ unpack o

instance PrettyShow SExpr where
  pshow = emptyAcc showExpr

emptyAcc :: (String -> String -> a -> [String]) -> a -> String
emptyAcc f = unlines . join f ""


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
showElem acc (List   es) = showList    acc es
showElem acc (Nested e ) = showExpr'   acc e
showElem acc e           = showAsChild acc e

showAsChild :: PrettyShow a => String -> a -> String
showAsChild acc a = withChld acc ++ pshow a

showWith :: (String -> String -> a -> [String]) -> String -> a -> String
showWith f acc    = unlines' . lifted f acc
  where lifted f' = liftA2 f' withChld withNxt
        unlines'  = intercalate "\n"

showExpr' :: String -> SExpr -> String
showExpr' = showWith showExpr

showList :: String -> [Element] -> String
showList = showWith showListElems

showListElems :: String -> String -> [Element] -> [String]
showListElems acc1 acc2 es = start : showElems es ++ end
  where showElems = map (showElem acc2)
        start     =  acc1 ++ "["
        end       = [acc2 ++ "]"]

