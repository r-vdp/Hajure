{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests where

import Hajure.AST (listify)
import Hajure.Data
import Hajure.Parsing ()
import Test.Hajure.DataArbitraries ()

-- |
-- prop> not . hasListFunctor . listify
hasListFunctor :: Element -> Bool
hasListFunctor (Nested (SExpr (e:es))) = e == Ident "list" ||
                                         any hasListFunctor es
hasListFunctor _                       = False

