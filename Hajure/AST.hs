{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hajure.AST
  ( listify
  , funify
  , rename
  ) where

import Control.Applicative

import Data.List (sort)
import Data.Traversable (traverse)

import Hajure.Data
import Hajure.Unique


-- $setup
-- >>> :set -XOverloadedStrings 

-- |
-- prop> listify s == (listify . listify) (s :: Element)
-- >>> let n  = Nested (mkSexpr [Ident "list", Num 3, Num 4])
-- >>> let n' = listify n
-- >>> listify n == n'
-- True
-- >>> print n'
-- List [Num 3.0,Num 4.0]

class AST a where
  listify :: a -> Element
  funify  :: a -> Element
  renameM :: a -> Unique a

instance AST Element where
  listify (Nested s) = listify s
  listify (List xs)  = List (map listify xs)
  listify e          = e

  funify (Nested s) = funify s
  funify (List xs)  = List (map funify xs)
  funify e          = e

  renameM (Nested s)   = Nested <$> (pushScope *> renameM s <* popScope)
  renameM (List xs)    = List <$> traverse renameM xs
  renameM (Ident i)    = Ident <$> nextUnique i
  renameM (Fun i is s) = withNew (Fun <$> nextUnique i) is (renameM s)
  renameM e            = pure e

instance AST SExpr where
  listify s@(sexprView -> (x:xs))
    | isList x  = List (map listify xs)
    | otherwise = Nested (listify <$> s)
  listify s     = Nested s

  funify s
    | Just f <- toDefun s = f
    | otherwise           = Nested (funify <$> s)

  renameM = traverse renameM

rename :: AST a => [a] -> ([a], Mappings)
rename = runUnique . traverse renameM

isList :: Element -> Bool
isList x
  | Ident i <- x
  , i == "list"  = True
  | otherwise    = False

-- TODO return error msg if argument list not unique (sort guard)
toDefun :: SExpr -> Maybe Element
toDefun s
  | [Ident d, Ident i, Nested (sexprView -> is'), Nested b] <- sexprView s
  , d == "defun"
  , Just is <- traverse toIdent is'
  , sort is == is               = Just (Fun i is (funify <$> b))
  | otherwise                   = Nothing

toIdent :: Element -> Maybe Identifier
toIdent (Ident i) = Just i
toIdent _         = Nothing

