{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hajure.DataArbitraries where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Hajure.Data

class Arbitrary' a where
  arbitrary' :: Int -> Gen a

instance Arbitrary' Element where
  arbitrary' f = frequency [ (2, Nested <$> arbitrary' f')
                           , (f, Ident  <$> letters)
                           , (f, Num    <$> arbitrary)
                           , (f, Op     <$> operator)
                           , (2, List   <$> elems f')
                           , (2, listFunctor f')
                           ]
    where f' = f * 5

instance Arbitrary' SExpr where
  arbitrary' f = SExpr <$> elems1 f

instance Arbitrary Element where
  arbitrary = arbitrary' 1

instance Arbitrary SExpr where
  arbitrary = arbitrary' 1


listFunctor :: Int -> Gen Element
listFunctor f = Nested . SExpr <$> listIdent <:> elems1 f
  where listIdent = pure . Ident $ "list"

elems :: Int -> Gen [Element]
elems = listOf . arbitrary'

elems1 :: Int -> Gen [Element]
elems1 = listOf1 . arbitrary'

letters :: Gen Text
letters = T.pack <$> initLetter <:> listOf letter

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

initLetter :: Gen Char
initLetter = elements (['a'..'z'] ++ ['A'..'Z'])

letter :: Gen Char
letter = frequency [(5,initLetter), (1,pure '_'), (1,pure '\'')]

operator :: Gen Text
operator = elements $ ["+", "-", "*", "/"]

