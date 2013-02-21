{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Tests where

import Hajure.AST (listify)
import Hajure.Data
import Hajure.Parsing ()
import Test.Hajure.DataArbitraries ()

-- |
-- prop> not . hasListFunctor . listify :: Element -> Bool
hasListFunctor :: Element -> Bool
hasListFunctor (Nested (sexprView -> (e:es))) = e == Ident "list" ||
                                                any hasListFunctor es
hasListFunctor _                              = False

