{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

module Hajure.Unique
  ( Unique
  , evalUnique
  , nextUnique
  , pushScope
  , popScope
  ) where

import Control.Applicative ((*>), Applicative)
import Control.Arrow (first, second)
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (pack, append)

import Hajure.Data


newtype Scope = Scope (Map Identifier Identifier)

type UState = (Integer, [Scope])

newtype Unique a = Unique { runUnique :: State UState a }
  deriving (Monad, Applicative, Functor)

evalUnique :: Unique a -> a
evalUnique = flip evalState empty . runUnique
  where empty = (-1, [])

nextUnique :: Identifier -> Unique Identifier
nextUnique i = do
  s <- getsState snd
  case findIdent i s of
    Just u -> return u
    _      -> do
      i' <- nextIdent
      modifyState (second (addIdent i i'))
      return i'

pushScope :: Unique ()
pushScope = modifyState (second (Scope M.empty :))

popScope :: Unique ()
popScope = modifyState (second (drop 1))

addIdent :: Identifier -> Identifier -> [Scope] -> [Scope]
addIdent i i' (Scope s : ss) = (Scope (M.insert i i' s)) : ss
addIdent i i' []             = [Scope (M.insert i i' M.empty)]

findIdent :: Identifier -> [Scope] -> Maybe Identifier
findIdent i (Scope s : ss)
  | Just u <- M.lookup i s = Just u
  | otherwise              = findIdent i ss
findIdent _ []             = Nothing

nextIdent :: Unique Identifier
nextIdent = modifyState (first (+1)) *> getsState asIdent
  where asIdent = ("$x" `append`) . pack . show . fst

modifyState :: (UState -> UState) -> Unique ()
modifyState = Unique . modify

getsState :: (UState -> a) -> Unique a
getsState = Unique . gets

