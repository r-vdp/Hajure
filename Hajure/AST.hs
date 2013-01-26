{-# LANGUAGE PatternGuards, OverloadedStrings #-}

module Hajure.AST (listify) where

import Hajure.Data

-- $setup
-- >>> :set -XOverloadedStrings 

-- |
-- >>> let n  = Nested (SExpr [Ident "list", Num 3, Num 4])
-- >>> listify n
-- List [Num 3.0,Num 4.0]
-- >>> (listify . listify) n
-- List [Num 3.0,Num 4.0]

listify :: Element -> Element
listify (Nested (SExpr xs'@(x:xs)))
  | Ident e <- x
  , e == "list"   = List (map listify xs)
  | otherwise     = Nested . SExpr $ map listify xs'
listify (List xs) = List (map listify xs)
listify e         = e

