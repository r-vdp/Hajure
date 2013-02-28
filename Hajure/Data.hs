{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hajure.Data
  ( Element(..)
  , SExpr
  , mkSexpr
  , sexprView
  , Mapping
  , Mappings(..)
  , Identifier
  , PrettyShow
  , pshow
  ) where

import Prelude hiding (showList)

import Control.Monad (join)

import Data.Foldable (Foldable)
import Data.List (intercalate)
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Traversable (Traversable)

import ParsecImports


type Identifier = Text

data Element = Nested SExpr
             | Ident  Identifier
             | Num    Double
             | Op     Text
             | List   [Element]
             | Fun    Identifier [Identifier] SExpr
  deriving (Eq, Show)

newtype Wrapped a = Wrapped { unwrap :: [a] }
  deriving (Show, Eq, Functor, Foldable, Traversable)

type SExpr = Wrapped Element

mkSexpr :: [Element] -> SExpr
mkSexpr = Wrapped

sexprView :: SExpr -> [Element]
sexprView = unwrap

type Mapping = (Identifier, Identifier)

newtype Mappings  = Mappings { toMappings :: [Mapping] }
  deriving (Show, Monoid)


class PrettyShow a where
  pshow :: a -> String

instance PrettyShow Mappings where
  pshow = unlines . map showMapping . toMappings
    where showMapping (i, i') = unpack i' ++ " -> " ++ unpack i

instance PrettyShow Element where
  pshow (Nested   s) = pshow s
  pshow (List     l) = emptyAcc showListElems l
  pshow (Ident    i) = "Ident " ++ unpack i
  pshow (Num      n) = "Num "   ++ show   n
  pshow (Op       o) = "Op "    ++ unpack o
  pshow (Fun i is s) = unlines' $ showFun "" "" i is s

instance PrettyShow SExpr where
  pshow = emptyAcc showExpr

emptyAcc :: (String -> String -> a -> [String]) -> a -> String
emptyAcc f = unlines' . join f ""


chld, nxt :: String
chld = "|-- "
nxt  = "|   "

unlines' :: [String] -> String
unlines' = intercalate "\n"


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
showElem acc (List es)    = showList    acc es
showElem acc (Nested e)   = showExpr'   acc e
showElem acc (Fun i is s) = showFun'    acc i is s
showElem acc e            = showAsChild acc e

showAsChild :: PrettyShow a => String -> a -> String
showAsChild acc a = withChld acc ++ pshow a

showWith :: (String -> String -> a -> [String]) -> String -> a -> String
showWith f acc    = unlines' . lifted f acc
  where lifted f' = liftA2 f' withChld withNxt

showList :: String -> [Element] -> String
showList = showWith showListElems

showExpr' :: String -> SExpr -> String
showExpr' = showWith showExpr

showFun' :: String -> Identifier -> [Identifier] -> SExpr -> String
showFun' acc i is = showWith (\acc1 acc2 -> showFun acc1 acc2 i is) acc

showListElems :: String -> String -> [Element] -> [String]
showListElems acc1 acc2 es = start : showElems es ++ end
  where showElems = map (showElem acc2)
        start     =  acc1 ++ "["
        end       = [acc2 ++ "]"]

showFun :: String -> String -> Identifier -> [Identifier] -> SExpr -> [String]
showFun acc1 acc2 i is s = [start, body, end]
  where start = acc1 ++ "Fun( " ++ unpack i ++ " " ++ args ++ " ->"
        args  = "[ " ++ (unwords . map unpack $ is) ++ " ]"
        body  = showExpr' acc2 s
        end   = acc2 ++ ")"

