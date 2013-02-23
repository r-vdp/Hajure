{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

module Hajure.Unique
  ( Unique
  , runUnique
  , nextUnique
  , withNew
  , pushScope
  , popScope
  ) where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (pack, append)
import Data.Traversable (traverse)

import Hajure.Data


newtype Scope = Scope (Map Identifier Identifier)

type UState = (Integer, [Scope])

newtype Unique a = Unique { runUnique' :: WriterT Mappings (State UState) a }
  deriving (Monad, Applicative, Functor)

runUnique :: Unique a -> (a, Mappings)
runUnique = flip evalState emptyState . runWriterT . runUnique'
  where emptyState = (-1, [])

nextUnique :: Identifier -> Unique Identifier
nextUnique i = do
  s <- getScopes
  maybe (newUnique i) return (findIdent i s)

withNew :: Unique ([Identifier] -> a -> b)
        -> [Identifier] -> Unique a -> Unique b
withNew f is m = f <* pushScope <*> traverse newUnique is <*> m <* popScope

pushScope :: Unique ()
pushScope = modifyScopes (Scope M.empty :)

popScope :: Unique ()
popScope = modifyScopes (drop 1)

newUnique :: Identifier -> Unique Identifier
newUnique i = do
  i' <- nextIdent
  addMapping i i'
  return i'

findIdent :: Identifier -> [Scope] -> Maybe Identifier
findIdent i (Scope s : ss) = M.lookup i s <|> findIdent i ss
findIdent _ []             = Nothing

nextIdent :: Unique Identifier
nextIdent = increment *> getsState asIdent
  where asIdent   = ("$x" `append`) . pack . show . fst
        increment = modifyState (first (+1))

modifyState :: (UState -> UState) -> Unique ()
modifyState = Unique . modify

modifyScopes :: ([Scope] -> [Scope]) -> Unique ()
modifyScopes = modifyState . second

getsState :: (UState -> a) -> Unique a
getsState = Unique . gets

tellMapping :: Mapping -> Unique ()
tellMapping = Unique . tell . Mappings . (:[])

getScopes :: Unique [Scope]
getScopes = getsState snd

addMapping :: Identifier -> Identifier -> Unique ()
addMapping i i' = tellMapping (i,i') *> modifyScopes (addIdent i i')

addIdent :: Identifier -> Identifier -> [Scope] -> [Scope]
addIdent i i' (Scope s : ss) = (Scope (M.insert i i' s)) : ss
addIdent i i' []             = [Scope (M.insert i i' M.empty)]

