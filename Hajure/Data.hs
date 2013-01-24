
module Hajure.Data where

import Data.List (intersperse)

data Element a = Nested (SExpr a)
               | Ident  a
               | Num    a
               | Op     a
               | List   [Element a]
  deriving Show

newtype SExpr a = SExpr { unwrap :: [Element a] }

instance Show a => Show (SExpr a) where
  show = unlines . showExpr "" ""
    where
      
      chld = "|-- "
      nxt  = "|   "

      showExpr :: Show a => String -> String -> SExpr a -> [String]
      showExpr acc1 acc2 e = start : body e ++ end
        where start = acc1 ++ "S("
              body  = map (showElem acc2) . unwrap
              end   = [acc2 ++ ")"]

      showElem :: Show a => String -> Element a -> String
      showElem acc (Ident a)  = acc ++ chld ++ show a
      showElem acc (Num a)    = acc ++ chld ++ show a
      showElem acc (Op a)     = acc ++ chld ++ show a
      showElem acc (List as)  = acc ++ chld ++ show as
      showElem acc (Nested e) = unlines' . showExpr (acc ++ chld) (acc ++ nxt) $ e
                                
      unlines' :: [String] -> String
      unlines' = concat . intersperse "\n"
 
